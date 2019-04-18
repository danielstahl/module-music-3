package net.soundmining

import net.soundmining.Instrument.{EnvCurve, TAIL_ACTION, setupNodes}
import net.soundmining.Instruments._
import net.soundmining.ModularInstrument.{AudioInstrument, ControlInstrument}
import net.soundmining.Note.noteToHertz
import net.soundmining.Spectrum.makeFmSynthesis
import net.soundmining.Utils.absoluteTimeToMillis
import Melody._

import scala.util.Random

object Arpeggio1 {
  def test1(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {

  }

  def main(args: Array[String]): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)

    test1(0f)
  }
}
