package com.rathertremendous.dota

import com.dota2.proto.Demo.CGameInfo.CDotaGameInfo.CHeroSelectEvent
import org.omg.CORBA.CurrentHelper
import skadistats.clarity.Clarity
import com.dota2.proto.Demo.CDemoFileInfo
import com.dota2.proto.Demo
import scala.collection.JavaConverters._
import scala.collection.mutable
import skadistats.clarity.parser.Profile
import skadistats.clarity.`match`.Match
import skadistats.clarity.model.GameEventDescriptor
import org.joda.time.format.PeriodFormatterBuilder
import org.joda.time.Duration
import java.io.File

object Constants {
  val RadiantGameTeam = 2
  val DireGameTeam = 3

  val GameTeamString = Map(RadiantGameTeam -> "Radiant", DireGameTeam -> "Dire")
  val FoursquareTeamId: Seq[Long] = Seq(2137863L)
  val FoursquareSteamIds: Seq[Long] = Seq(76561198024953671L, 76561197988204982L, 76561198032953138L, 76561197968130680L, 76561197993400479L)

  val MicrosoftclippyTeamIds: Seq[Long] = Vector(1983359L, 2114405L)
  val MicrosoftclippySteamIds: Seq[Long] = Vector(76561198049392190L, 76561197980133575L, 76561198022420821L, 76561198031713102L, 76561197982116168L, 76561197999661103L, 76561198025580907L, 76561197965646923L, 76561198000663657L, 76561198031119451L, 76561198031120885L)

  val AthenahealthTeamIds: Seq[Long] = Vector(1950879)
  val AthenahealthSteamIds: Seq[Long] = Vector(76561198006252704L, 76561198004080999L, 76561198042549753L, 76561198013112786L, 76561198047042794L, 76561198000728942L, 76561197982116168L, 76561197999661103L, 76561197965646923L, 76561198000663657L, 76561198022420821L)

  val FacebookSlarkTeamIds: Seq[Long] = Vector(2041295L)
  val FacebookSlarkSteamIds: Seq[Long] = Vector(76561198014066692L, 76561197961131547L, 76561197980082827L, 76561197999544403L, 76561197991154564L, 76561197996746910L, 76561198002972277L)
}

object DotaReplaySummarizer {
  import Constants._

  def otherTeam(gameTeam: Int) = {
    if (gameTeam == RadiantGameTeam) {
      DireGameTeam
    } else if (gameTeam == DireGameTeam) {
      RadiantGameTeam
    } else {
      throw new Exception(s"${gameTeam} is not a known gameTeam")
    }
  }


  def main(args: Array[String]) = {
    val dir = "/Users/jonshea/Desktop/facebookslark"
    val teamIds: Seq[Long] = FacebookSlarkTeamIds
    // Include steam IDs of players on the team you care about, in case they didn't set their team id
    val knownSteamIds: Seq[Long] = FacebookSlarkSteamIds

    val games = for {
      file <- (new File(dir)).listFiles
      if file.isFile && file.getName.endsWith(".dem")
      summary <- teamSummariesForFile(file.getAbsolutePath)
    } yield {
        if (teamIds.isEmpty) {
          println(s"Team ${summary.teamTag}: ${summary.teamId}")
        }
      summary
    }

    val finalSummary = summarizeGamesForTeam(games, teamIds, knownSteamIds)
    println(finalSummary)
  }

  def teamSummariesForFile(filename: String): Seq[TeamSummary] = {
    val gameNumber = filename.split("/").last.split("_").toSeq.headOption.getOrElse("0").toInt
    val info = Clarity.infoForFile(filename)
    val dota  = info.getGameInfo.getDota
    val demo = CDemoFileInfo.getDefaultInstance
    val gameInfo = demo.getGameInfo

    val teamByTag = Map(
      RadiantGameTeam -> new TeamSummary(),
      DireGameTeam -> new TeamSummary()
    )

    for {
      (gameTeam, teamSummary) <- teamByTag
    } {
      teamSummary.gameNumber = gameNumber
      teamSummary.matchId = dota.getMatchId
      teamSummary.gameTeam = gameTeam
      teamSummary.won = dota.getGameWinner == gameTeam

      if (gameTeam == RadiantGameTeam) {
        teamSummary.teamId = dota.getRadiantTeamId
        teamSummary.teamTag = dota.getRadiantTeamTag
        teamSummary.opponentTag = dota.getDireTeamTag
      } else if (gameTeam == DireGameTeam) {
        teamSummary.teamId = dota.getDireTeamId
        teamSummary.teamTag = dota.getDireTeamTag
        teamSummary.opponentTag = dota.getRadiantTeamTag
      }
    }

    for {
      pickBan: CHeroSelectEvent <- dota.getPicksBansList.asScala
    } {
      if (pickBan.getIsPick) {
        teamByTag(pickBan.getTeam).picks += pickBan.getHeroId
      } else {
        teamByTag(pickBan.getTeam).bans += pickBan.getHeroId
        teamByTag(otherTeam(pickBan.getTeam)).opponentBans += pickBan.getHeroId
      }
    }

    for {
      player <- dota.getPlayerInfoList.asScala
    } {
      teamByTag(player.getGameTeam).steamIds += player.getSteamid
      teamByTag(player.getGameTeam).players += (Heroes.idFromName(player.getHeroName) -> player.getPlayerName)
    }

    teamByTag.values.toSeq
  }

  def summarizeGamesForTeam(
    manyGames: Seq[TeamSummary],
    teamIds: Seq[Long],
    knownSteamIds: Seq[Long]
    ): String = {
    val knownTeamIdsSet: Set[Long] = teamIds.toSet
    val knownSteamIdSet: Set[Long] = knownSteamIds.toSet

    val games = manyGames.filter(g => {
      knownTeamIdsSet.contains(g.teamId) ||
        g.steamIds.exists(knownSteamIdSet.contains(_))
    }).sortBy(g => -1 * g.gameNumber)

    val foundTeamIds = {
      games.map(_.teamId).distinct.filterNot(_ == 0)
    }

    val foundSteamIds = {
      games.flatMap(_.steamIds).distinct.mkString("", "L, ", "L")
    }

    val heroPickCountsSummary = {
      heroCountSummary(games.flatMap(_.picks))
        .map(formatHeroCountSummary(_))
        .mkString("\n")
    }
    val heroBanCountsSummary = {
      heroCountSummary(games.flatMap(_.bans))
        .filter(_._2 > 1)
        .map(formatHeroCountSummary(_))
        .mkString("\n")
    }

    "Team Ids: " + foundTeamIds +
      "\n\n" +
    "Steam Ids: " + foundSteamIds +
    "\n\n" +
      "Hero Pick Summary\n" +
      heroPickCountsSummary +
      "\n\n" +
      "Hero Ban Summary\n" +
      heroBanCountsSummary +
      "\n\n" +
      games.map(_.asString).mkString("\n")
  }

  def heroCountSummary(heroIds: Seq[Int]): Seq[(Int, Int)] = {
    heroIds
      .groupBy(identity)
      .map({case (heroId, l) => (heroId -> l.length)})
      .toSeq
      .sortBy(-1 * _._2)
  }

  def formatHeroCountSummary(x: (Int, Int)): String = {
    val (heroId, pickCount) = x
    s"${Heroes.byId(heroId)}: ${pickCount}"
  }


  // Does not work.
  def lastHitCounter(filename: String) = {
    val GAMETIME_FORMATTER = {
      new PeriodFormatterBuilder()
        .minimumPrintedDigits(2)
        .printZeroAlways()
        .appendMinutes()
        .appendLiteral(":")
        .appendSeconds()
        .appendLiteral(".")
        .appendMillis3Digit()
        .toFormatter()
    }


    val iter = Clarity.tickIteratorForFile(filename, Profile.COMBAT_LOG).asScala
    val gameMatch = new Match()
    val combatLogDescriptor = gameMatch.getGameEventDescriptors.forName("dota_combatlog")

    for {
      i <- iter
      _ = i.apply(gameMatch)
      ge <- gameMatch.getGameEvents.asScala
      if ge.getProperty(combatLogDescriptor.getIndexForKey("type")).asInstanceOf[Int] == 4
    } {

      val timestamp = ge.getProperty(combatLogDescriptor.getIndexForKey("timestamp")).asInstanceOf[Float]
      val time = "[" + GAMETIME_FORMATTER.print(Duration.millis((1000.0f * timestamp).toInt).toPeriod()) +  "]"
      val targetName = ge.getProperty(combatLogDescriptor.getIndexForKey("targetname")).asInstanceOf[String]
      val attackerName = ge.getProperty(combatLogDescriptor.getIndexForKey("attackername")).asInstanceOf[String]
      println(s"${time} ${targetName} is killed by ${attackerName}")

    }
  }
}


class TeamSummary() {
  import Constants._

  var gameNumber: Int = 0
  var matchId: Long = -1
  var gameTeam: Int = -1
  var teamTag: String = ""
  var opponentTag: String = ""
  var teamId: Long = 0
  var won: Boolean = false
  var picks: mutable.Buffer[Int] = mutable.Buffer.empty
  var bans: mutable.Buffer[Int] = mutable.Buffer.empty
  var opponentBans: mutable.Buffer[Int] = mutable.Buffer.empty
  var players: mutable.Map[Int, String] = mutable.Map.empty[Int, String]
  var steamIds: mutable.Buffer[Long] = mutable.Buffer.empty

  def asString: String  = {
    val lines = mutable.Buffer.empty[String]

    val wonLostStr = if (won) "Winner" else "loser"

    lines += s"Game ${gameNumber}: ${teamTag} (${GameTeamString(gameTeam)}) v. ${opponentTag}"
    lines += wonLostStr

    for {
      heroId <- picks
    } {
      lines += s"${Heroes.byId(heroId)} (${players(heroId)})"
    }
    lines += "Bans: " + bans.map(Heroes.byId(_)).mkString("", ", ", "")
    lines += "Opponent Bans: " + opponentBans.map(Heroes.byId(_)).mkString("", ", ", "")

    lines.mkString("\n") + "\n"
  }
}