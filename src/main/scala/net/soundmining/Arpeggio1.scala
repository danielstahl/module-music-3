package net.soundmining

import net.soundmining.Instrument.{EnvCurve, TAIL_ACTION, setupNodes, EFFECT}
import net.soundmining.Instruments._
import net.soundmining.ModularInstrument.{AudioInstrument, ControlInstrument, StaticAudioBusInstrument}
import net.soundmining.Note.noteToHertz
import net.soundmining.Spectrum._
import net.soundmining.Utils.absoluteTimeToMillis
import Melody._
import scala.io.StdIn
import scala.util.Random

/*
Maybe some kind of dialogue. Question, response. And some background that is not moving.
Theme and dialouge over an ambient background. Lou Reed, "me, I just don't care at all".
*/
object Arpeggio1 {

  /*
  Play with attackTime/duration
  0.05 / 0.1
  0.05 / 0.9
  0.001 / 0.002
  0.001 / 0.02
  */
  def playSine(startTime: Float, freq: Float, output: StaticAudioBusInstrument)(implicit player: MusicPlayer): Unit = {
    val amp = percControl(0.01f, 1.0f, 0.02f, Right(Instrument.LINEAR))
    val freqControl = staticControl(freq)
    val osc = sineOsc(amp, freqControl)
      .withOutput(output)
      .addAction(TAIL_ACTION)
    
    val graph = osc.buildGraph(startTime, 0.1f, osc.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(startTime), graph)  
  }

  def playFm(startTime: Float, duration: Float, ampValue: Float, attackTime: Float, modFreq: Float, carrierFreq: Float, modAmount: (Float, Float), modAttackTime: Float, output: StaticAudioBusInstrument)(implicit player: MusicPlayer): Unit = {
    val amp = percControl(0.01f, ampValue, attackTime, Right(Instrument.SINE))
    val modAmountControl = percControl(modAmount._1, modAmount._2, modAttackTime, Right(Instrument.SINE))
    val modulator = sineOsc(modAmountControl, staticControl(modFreq))
    val fm = fmSineModulate(staticControl(carrierFreq), modulator, amp)
      .withOutput(output)
      .addAction(TAIL_ACTION)
    val graph = fm.buildGraph(startTime, duration, fm.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(startTime), graph)  
  }

  def simpleFm(startTime: Float, duration: Float, ampValue: Float, attackTime: Float, modFact: Float, carrierFreq: Float, modAmount: (Float, Float), modAttackTime: Float, panValue: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val modFreq = carrierFreq * modFact
    val amp = percControl(0.01f, ampValue, attackTime, Right(Instrument.LINEAR))
    val modAmountControl = percControl(modAmount._1, modAmount._2, modAttackTime, Right(Instrument.LINEAR))
    val modulator = sineOsc(modAmountControl, staticControl(modFreq))
    val fm = fmSineModulate(staticControl(carrierFreq), modulator, amp)
      .addAction(TAIL_ACTION)

    val pan = panning(fm, staticControl(panValue))
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)
      
    pan.getOutputBus.staticBus(0)    
    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(startTime), graph)  
  }

  def test1(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val fact = makeFact(noteToHertz('c2), noteToHertz('fiss3))
    val spectrum = makeSpectrum2(noteToHertz('c2), fact, 50)

    val delayAudioBus = staticAudioBus()
    val delayAmp = staticControl(1)
    val delay = monoDelay(delayAudioBus, delayAmp, delayTime = 0.60f, decayTime = 3f)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    val pan = panning(delay, staticControl(0f))
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)
      
    pan.getOutputBus.staticBus(0)  
    val graph = pan.buildGraph(startTime, 10f, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(startTime), graph)

    val times = absolute(startTime, 
      Seq(0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f))

    playSine(times.head, spectrum(10), delayAudioBus)
    playSine(times(1), spectrum(13), delayAudioBus)
    playSine(times(2), spectrum(16), delayAudioBus)
    playSine(times(3), spectrum(19), delayAudioBus)
    playSine(times(4), spectrum(10), delayAudioBus)
    playSine(times(5), spectrum(13), delayAudioBus)
    playSine(times(6), spectrum(16), delayAudioBus)
    playSine(times(7), spectrum(19), delayAudioBus)
    playSine(times(8), spectrum(10), delayAudioBus)
    playSine(times(9), spectrum(13), delayAudioBus)
    playSine(times(10), spectrum(16), delayAudioBus)
    playSine(times(11), spectrum(19), delayAudioBus)
    playSine(times(12), spectrum(10), delayAudioBus)
    playSine(times(13), spectrum(13), delayAudioBus)
    playSine(times(14), spectrum(16), delayAudioBus)
  }

  def test2(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val fact = makeFact(noteToHertz('c2), noteToHertz('fiss3))
    val spectrum = makeSpectrum2(noteToHertz('c2), fact, 50)

    val delayAudioBus = staticAudioBus()
    val delayAmp = staticControl(0.5f)
    val delay = monoDelay(delayAudioBus, delayAmp, delayTime = 0.60f, decayTime = 2f)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    val filterFreqBus = sineControl(staticControl(0.1f), 500, 4000)

    val filterGainBus = sineControl(staticControl(0.1f), 3.5f, 1f)
    val moog = moogFilter(delay, filterFreqBus, filterGainBus)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)
      
    val pan = panning(moog, staticControl(0f))
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)
      
    pan.getOutputBus.staticBus(0)  
    val graph = pan.buildGraph(startTime, 10f, pan.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(startTime), graph)

    val times = absolute(startTime, 
      Seq(0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f))

    val fact2 = spectrum(1) / spectrum.head   
    val fact3 = spectrum(2) / spectrum.head
    val fact4 = spectrum(3) / spectrum.head

    playFm(times.head, .3f, 0.5f, 0.03f, spectrum(10) * fact, spectrum(10), (4000, 5000), 0.02f, delayAudioBus)
    playFm(times(1), .3f, 0.5f, 0.03f, spectrum(14) * fact, spectrum(14), (4000, 5000), 0.02f, delayAudioBus)
    playFm(times(2), .3f, 0.5f, 0.03f, spectrum(17) * fact, spectrum(17), (4000, 5000), 0.02f, delayAudioBus)
    playFm(times(3), .3f, 0.5f, 0.03f, spectrum(16) * fact, spectrum(16), (4000, 5000), 0.02f, delayAudioBus)
    
    playFm(times(4), .3f, 0.5f, 0.03f, spectrum(10) * fact, spectrum(10), (4000, 5000), 0.02f, delayAudioBus)
    playFm(times(5), .3f, 0.5f, 0.03f, spectrum(14) * fact, spectrum(14), (4000, 5000), 0.02f, delayAudioBus)
    playFm(times(6), .3f, 0.5f, 0.03f, spectrum(17) * fact, spectrum(17), (4000, 5000), 0.02f, delayAudioBus)
    playFm(times(7), .3f, 0.5f, 0.03f, spectrum(16) * fact, spectrum(16), (4000, 5000), 0.02f, delayAudioBus)

    playFm(times(8), .3f, 0.5f, 0.03f, spectrum(10) * fact, spectrum(10), (4000, 5000), 0.02f, delayAudioBus)
    playFm(times(9), .3f, 0.5f, 0.03f, spectrum(14) * fact, spectrum(14), (4000, 5000), 0.02f, delayAudioBus)
    playFm(times(10), .3f, 0.5f, 0.03f, spectrum(17) * fact, spectrum(17), (4000, 5000), 0.02f, delayAudioBus)
    playFm(times(11), .3f, 0.5f, 0.03f, spectrum(16) * fact, spectrum(16), (4000, 5000), 0.02f, delayAudioBus)
    
      /*
    playSine(times.head, spectrum(10), delayAudioBus)
    playSine(times(1), spectrum(13), delayAudioBus)
    playSine(times(2), spectrum(16), delayAudioBus)
    playSine(times(3), spectrum(19), delayAudioBus)
    playSine(times(4), spectrum(10), delayAudioBus)
    playSine(times(5), spectrum(13), delayAudioBus)
    playSine(times(6), spectrum(16), delayAudioBus)
    playSine(times(7), spectrum(19), delayAudioBus)
    playSine(times(8), spectrum(10), delayAudioBus)
    playSine(times(9), spectrum(13), delayAudioBus)
    playSine(times(10), spectrum(16), delayAudioBus)
    playSine(times(11), spectrum(19), delayAudioBus)
    playSine(times(12), spectrum(10), delayAudioBus)
    playSine(times(13), spectrum(13), delayAudioBus)
    playSine(times(14), spectrum(16), delayAudioBus)
    */
  }

  /*
  Explore harmonies with different spectras as chord progressions.
  Perhaps two contrasting spectras at the same time.
  Long melodies with ornament that have short notes.
  Parhaps canon at different tempos. Very slowly and very fast.
  */
  def test3(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val facts = Seq(
      makeFact(noteToHertz('c2), noteToHertz('fiss3)),
      makeFact(noteToHertz('diss2), noteToHertz('d3)),
      makeFact(noteToHertz('hess2), noteToHertz('g3)),
      makeFact(noteToHertz('ciss2), noteToHertz('diss3)),
      makeFact(noteToHertz('h1), noteToHertz('giss3)),
      makeFact(noteToHertz('diss2), noteToHertz('a3)),
      makeFact(noteToHertz('d2), noteToHertz('f3))
    )

    val f1 = makeFact(noteToHertz('c2), noteToHertz('fiss3))
    val spectrum1 = makeSpectrum2(noteToHertz('c2), f1, 50)
    val f12 = spectrum1(1) / spectrum1.head
    val f13 = spectrum1(2) / spectrum1.head

    println(f1)
    println(f12)
    println(f13)

    simpleFm(0, 20f, 0.5f, 10f, f12, spectrum1.head, (100, 500), 10f, 0f)

    simpleFm(0, 5f, 0.5f, 2.5f, f1, spectrum1(3), (100, 3000), 2.5f, -0.5f)
    simpleFm(0, 5f, 0.5f, 2.5f, f1, spectrum1(4), (100, 3000), 2.5f, 0f)
    simpleFm(0, 5f, 0.5f, 2.5f, f1, spectrum1(5), (100, 3000), 2.5f, 0.5f)

    simpleFm(5, 5f, 0.5f, 2.5f, f1, spectrum1(4), (100, 3000), 2.5f, -0.5f)
    simpleFm(5, 5f, 0.5f, 2.5f, f1, spectrum1(6), (100, 3000), 2.5f, 0f)
    simpleFm(5, 5f, 0.5f, 2.5f, f1, spectrum1(8), (100, 3000), 2.5f, 0.5f)

    simpleFm(10, 5f, 0.5f, 2.5f, f13, spectrum1(2), (100, 3000), 2.5f, -0.5f)
    simpleFm(10, 5f, 0.5f, 2.5f, f13, spectrum1(4), (100, 3000), 2.5f, 0f)
    simpleFm(10, 5f, 0.5f, 2.5f, f13, spectrum1(6), (100, 3000), 2.5f, 0.5f)

    simpleFm(15, 5f, 0.5f, 2.5f, f12, spectrum1(2), (100, 3000), 2.5f, -0.5f)
    simpleFm(15, 5f, 0.5f, 2.5f, f12, spectrum1(3), (100, 3000), 2.5f, 0f)
    simpleFm(15, 5f, 0.5f, 2.5f, f12, spectrum1(5), (100, 3000), 2.5f, 0.5f)
    /*
    simpleFm(0, 5f, 0.5f, 2.5f, f1, spectrum1(3), (100, 3000), 2.5f)
    simpleFm(5, 5f, 0.5f, 2.5f, f1, spectrum1(4), (100, 3000), 2.5f)
    simpleFm(10, 5f, 0.5f, 2.5f, f1, spectrum1(5), (100, 3000), 2.5f)

    simpleFm(15, 5f, 0.5f, 2.5f, f2, spectrum1(3), (100, 3000), 2.5f)
    simpleFm(20, 5f, 0.5f, 2.5f, f2, spectrum1(4), (100, 3000), 2.5f)
    simpleFm(25, 5f, 0.5f, 2.5f, f2, spectrum1(5), (100, 3000), 2.5f)

    simpleFm(30, 5f, 0.5f, 2.5f, f3, spectrum1(3), (100, 3000), 2.5f)
    simpleFm(35, 5f, 0.5f, 2.5f, f3, spectrum1(4), (100, 3000), 2.5f)
    simpleFm(40, 5f, 0.5f, 2.5f, f3, spectrum1(5), (100, 3000), 2.5f)
    */
  }

  def test4(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val baseNotes = Seq(
      (noteToHertz('c2), noteToHertz('fiss3)),
      (noteToHertz('diss2), noteToHertz('d3)), ///
      (noteToHertz('hess2), noteToHertz('g3)),
      (noteToHertz('ciss2), noteToHertz('diss3)),
      (noteToHertz('h1), noteToHertz('giss3)),
      (noteToHertz('diss2), noteToHertz('a3)),
      (noteToHertz('d2), noteToHertz('f3))
    )

    val facts = baseNotes.map {
      case (baseNote, octave) => makeFact(baseNote, octave)
    }
    
    val spectrums = baseNotes zip facts map {
      case ((baseNote, octave), fact) => makeSpectrum2(baseNote, fact, 50)
    }
    
    spectrums.zipWithIndex.map {
      case (spec, i) => 
        val start1 = i * 20f
        
        simpleFm(start1, 10f, 0.5f, 5f, facts(i), spec.head, (100, 500), 5f, 0f)
      
        simpleFm(start1, 5f, 0.5f, 2.5f, facts(i), spec(3), (100, 3000), 2.5f, -0.5f)
        simpleFm(start1, 5f, 0.5f, 2.5f, facts(i), spec(4), (100, 3000), 2.5f, 0f)
        simpleFm(start1, 5f, 0.5f, 2.5f, facts(i), spec(5), (100, 3000), 2.5f, 0.5f)

        val start2 = start1 + 5f
        val fac2 = spec(1) / spec.head
        simpleFm(start2, 5f, 0.5f, 2.5f, fac2, spec(3), (100, 3000), 2.5f, -0.5f)
        simpleFm(start2, 5f, 0.5f, 2.5f, fac2, spec(4), (100, 3000), 2.5f, 0f)
        simpleFm(start2, 5f, 0.5f, 2.5f, fac2, spec(5), (100, 3000), 2.5f, 0.5f)

        val start3 = start2 + 5f
        val fac3 = spec(2) / spec.head

        simpleFm(start3, 10f, 0.5f, 5f, facts(i), spec(1), (100, 500), 5f, 0f)

        simpleFm(start3, 5f, 0.5f, 2.5f, fac3, spec(3), (100, 3000), 2.5f, -0.5f)
        simpleFm(start3, 5f, 0.5f, 2.5f, fac3, spec(4), (100, 3000), 2.5f, 0f)
        simpleFm(start3, 5f, 0.5f, 2.5f, fac3, spec(5), (100, 3000), 2.5f, 0.5f)
    }
  }

  def threeBlock(lengths: (Float, Float, Float), vals: (Float, Float, Float, Float)): ThreeBlockControl = {
    threeBlockcontrol(
      startValue1 =  vals._1, len1 = lengths._1, 
      startValue2 = vals._2, len2 = lengths._2, 
      startValue3 = vals._3, len3 = lengths._3, 
      endValue3 = vals._4, 
      Right(Instrument.LINEAR))

  }

  /**
   * Explore low tones
  */
  def test5(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val (baseNote, octave) = (noteToHertz('c2), noteToHertz('fiss3))
    val fact = makeFact(baseNote, octave)
    val spectrum = makeSpectrum2(baseNote, fact, 50)
    
    val fact2 = spectrum(1) / spectrum.head
    val fact3 = spectrum(2) / spectrum.head
    val spectrum2 = makeSpectrum2(fact2, fact, 50)

    println(s"fact $fact fact2 $fact2 fact3 $fact3")
    println(s"spectrum $spectrum")
    println(s"spectrum2 $spectrum2")

    /*
    modFm(startTime, 0.2f, 0.5f, 0.01f, (spectrum(15) * fact2, spectrum(15) * fact), (spectrum(15), spectrum(15)), (2000, 5000), 0.01f, -0.5f)
    modFm(startTime + 0.5f, 0.2f, 0.5f, 0.01f, (spectrum(20) * fact2, spectrum(20) * fact), (spectrum(20), spectrum(20)), (2000, 5000), 0.01f, 0f)
    modFm(startTime + 1.0f, 0.2f, 0.5f, 0.01f, (spectrum(25) * fact2, spectrum(25) * fact), (spectrum(25), spectrum(25)), (2000, 5000), 0.01f, 0.5f)
    */
    simpleFm(0, 20f, 0.5f, 10f, fact, spectrum2(3), (spectrum(10), spectrum(35)), 10f, -0.5f)

    simpleFm(0, 20f, 0.5f, 10f, fact2, spectrum2(5), (spectrum(45), spectrum(15)), 10f, 0f)

    simpleFm(0, 20f, 0.5f, 10f, fact3, spectrum2(7), (spectrum(26), spectrum(46)), 10f, 0.5f)

    simpleFm(10, 20f, 0.5f, 10f, fact, spectrum(3), (spectrum(10), spectrum(35)), 10f, -0.5f)

    simpleFm(10, 20f, 0.5f, 10f, fact2, spectrum(5), (spectrum(45), spectrum(15)), 10f, 0f)

    simpleFm(10, 20f, 0.5f, 10f, fact3, spectrum(7), (spectrum(26), spectrum(46)), 10f, 0.5f)
  }

  /*
  Explore low tones for all spectrum
  */
  def test6(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val baseNotes = Seq(
      (noteToHertz('c2), noteToHertz('fiss3)),
      (noteToHertz('diss2), noteToHertz('d3)), ///
      (noteToHertz('hess2), noteToHertz('g3)),
      (noteToHertz('ciss2), noteToHertz('diss3)),
      (noteToHertz('h1), noteToHertz('giss3)),
      (noteToHertz('diss2), noteToHertz('a3)),
      (noteToHertz('d2), noteToHertz('f3)))

    val facts = baseNotes.map {
      case (baseNote, octave) => makeFact(baseNote, octave)
    }
    
    val spectrums = baseNotes zip facts map {
      case ((baseNote, octave), fact) => makeSpectrum2(baseNote, fact, 50)
    }

    val spectrum2s = spectrums zip facts map {
      case (spectrum, fact) => makeSpectrum2(spectrum(1) / spectrum.head, fact, 50)
    }

    spectrums.zipWithIndex.map {
      case (spectrum, i) =>
        val spectrum2 = spectrum2s(i) 
        val start1 = i * 20f
        val fact = facts(i)
        val fact2 = spectrum(1) / spectrum.head
        val fact3 = spectrum(2) / spectrum.head
        
        simpleFm(start1, 20f, 0.5f, 10f, fact, spectrum2(3), (spectrum(10), spectrum(35)), 10f, -0.5f)
        simpleFm(start1, 20f, 0.5f, 10f, fact2, spectrum2(5), (spectrum(45), spectrum(15)), 10f, 0f)
        simpleFm(start1, 20f, 0.5f, 10f, fact3, spectrum2(7), (spectrum(26), spectrum(46)), 10f, 0.5f)
    }
  }

  case class Note(startTime: Float, duration: Float, lengths: (Float, Float, Float)) {
    var audio: Option[AudioInstrument] = None
    var pan: Option[Panning] = None
    var output: Option[StaticAudioBusInstrument] = None

    def fm(ampValue: (Float, Float), modFreq: (Float, Float, Float, Float), carrierFreq: (Float, Float, Float, Float), modAmount: (Float, Float, Float, Float)): Note = {
      val amp = threeBlock(lengths = lengths, vals = (0.001f, ampValue._1, ampValue._2, 0.001f))
      val modAmountControl = threeBlock(lengths = lengths, vals = modAmount)
      
      val modFreqControl = threeBlock(lengths = lengths, vals = modFreq)
      val modulator = sineOsc(modAmountControl, modFreqControl)
      val carrierFreqControl = threeBlock(lengths = lengths, vals = carrierFreq)
      val fm = fmSineModulate(carrierFreqControl, modulator, amp)
        .addAction(TAIL_ACTION)

      audio = Some(fm)  
      this
    }

    def fmPulse(ampValue: (Float, Float), modFreq: (Float, Float, Float, Float), carrierFreq: (Float, Float, Float, Float), modAmount: (Float, Float, Float, Float)): Note = {
      val amp = threeBlock(lengths = lengths, vals = (0.001f, ampValue._1, ampValue._2, 0.001f))
      val modAmountControl = threeBlock(lengths = lengths, vals = modAmount)
      
      val modFreqControl = threeBlock(lengths = lengths, vals = modFreq)
      val modulator = pulseOsc(modAmountControl, modFreqControl)
      val carrierFreqControl = threeBlock(lengths = lengths, vals = carrierFreq)
      val fm = fmPulseModulate(carrierFreqControl, modulator, amp)
        .addAction(TAIL_ACTION)

      audio = Some(fm)  
      this
    }

    def pulse(ampValue: (Float, Float), freq: (Float, Float, Float, Float)): Note = {
      val amp = threeBlock(lengths = lengths, vals = (0.001f, ampValue._1, ampValue._2, 0.001f))
      val freqControl = threeBlock(lengths = lengths, vals = freq)
      val pulse = pulseOsc(ampBus = amp, freqBus = freqControl)
        .addAction(TAIL_ACTION)

      audio = Some(pulse)
      this  
    }

    def ring(ringModFreq: (Float, Float, Float, Float)): Note = {
      audio.foreach(a => {
        val ringModFreqControl = threeBlock(lengths = lengths, vals = ringModFreq)
        val ringMod = ringModulate(a, ringModFreqControl)
          .addAction(TAIL_ACTION)
        audio = Some(ringMod)  
      })
      this
    }

    def highPass(filterFreq: (Float, Float, Float, Float)): Note = {
      audio.foreach(a => {
        val filterFreqControl = threeBlock(lengths = lengths, vals = filterFreq)
        val filter = highPassFilter(a, freqBus = filterFreqControl)
          .addAction(TAIL_ACTION)
        audio = Some(filter)  
      })
      this
    }

    def lowPass(filterFreq: (Float, Float, Float, Float)): Note = {
      audio.foreach(a => {
        val filterFreqControl = threeBlock(lengths = lengths, vals = filterFreq)
        val filter = lowPassFilter(a, freqBus = filterFreqControl)
          .addAction(TAIL_ACTION)
        audio = Some(filter)  
      })
      this
    }

    def bandPass(lowerFreq: (Float, Float, Float, Float), higherFreq: (Float, Float, Float, Float)): Note = {
      // http://www.sengpielaudio.com/calculator-bandwidth.htm
      // http://www.sengpielaudio.com/calculator-geommean.htm
      // From the SuperCollider documentation  
      // The reciprocal of Q. Q is conventionally defined as cutoffFreq / bandwidth, meaning rq = (bandwidth / cutoffFreq).  

      def geometricMean(lower: Float, higher: Float): Float = math.sqrt(lower * higher).toFloat

      def center(lower: Float, higher: Float): Float = geometricMean(lower, higher)

      def rq(lower: Float, higher: Float): Float =
        (higher - lower) / lower

      val centerFreq = (
        center(lowerFreq._1, higherFreq._1), 
        center(lowerFreq._2, higherFreq._2), 
        center(lowerFreq._3, higherFreq._3), 
        center(lowerFreq._4, higherFreq._4))

      val rqs = (
        rq(lowerFreq._1, higherFreq._1), 
        rq(lowerFreq._2, higherFreq._2), 
        rq(lowerFreq._3, higherFreq._3), 
        rq(lowerFreq._4, higherFreq._4))  
      
        audio.foreach(a => {
          val freqControl = threeBlock(lengths = lengths, vals = centerFreq)
          val rqControl = threeBlock(lengths = lengths, vals = rqs)
          val filter = bandPassFilter(a, freqBus = freqControl, rqBus = rqControl)
            .addAction(TAIL_ACTION)
          audio = Some(filter)  
        })
        this
    }

    def pan(panValue: (Float, Float, Float, Float), output: Option[StaticAudioBusInstrument] = None): Note = {
      audio.map(a => {
        val panControl = threeBlock(lengths = lengths, vals = panValue)  
        val pan = panning(a, panControl)
          .addAction(TAIL_ACTION)
        if(output.isDefined) {
          pan.withOutput(output.get)
        } else {
          pan.getOutputBus.staticBus(0)    
        } 
        
        this.pan = Some(pan)
      })
      this
    }

    def play(implicit player: MusicPlayer): Unit = {
      pan.map(p => {
        val graph = p.buildGraph(startTime, duration, p.graph(Seq()))
        player.sendNew(absoluteTimeToMillis(startTime), graph)  
      })
    }
  }


  /*
  Test fm with threeblock controls. Middle theme.
  */
  def test7(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val (baseNote, octave) = (noteToHertz('c2), noteToHertz('fiss3))
    val fact = makeFact(baseNote, octave)
    val spectrum = makeSpectrum2(baseNote, fact, 50)
    
    val fact2 = spectrum(1) / spectrum.head
    val fact3 = spectrum(2) / spectrum.head
    val spectrum2 = makeSpectrum2(fact2, fact, 50)

    println(s"fact $fact fact2 $fact2 fact3 $fact3")
    println(s"spectrum $spectrum")
    println(s"spectrum2 $spectrum2")

    Note(startTime = startTime, duration = 8, lengths = (2, 5, 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq =   (spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact),
          carrierFreq = (spectrum(3), spectrum(3), spectrum(3), spectrum(3)),
          modAmount = (100, 300, 3000, 300))
       .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
       .play

    Note(startTime = startTime, duration = 8, lengths = (1, 5, 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact),
          carrierFreq = (spectrum(4), spectrum(4), spectrum(4), spectrum(4)),
          modAmount = (100, 2000, 500, 100))   
       .pan(panValue = (-0.8f, 0, 0, 0.8f))
       .play  
       
    Note(startTime = startTime, duration = 8, lengths = (2, 5, 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact),
          carrierFreq = (spectrum(5), spectrum(5), spectrum(5), spectrum(5)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play
  }

  /*
  Lower theme.
  */
  def test8(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val (baseNote, octave) = (noteToHertz('c2), noteToHertz('fiss3))
    val fact = makeFact(baseNote, octave)
    val spectrum = makeSpectrum2(baseNote, fact, 50)
    
    val fact2 = spectrum(1) / spectrum.head
    val fact3 = spectrum(2) / spectrum.head
    val spectrum2 = makeSpectrum2(fact2, fact, 50)

    println(s"fact $fact fact2 $fact2 fact3 $fact3")
    println(s"spectrum $spectrum")
    println(s"spectrum2 $spectrum2")

    Note(startTime = startTime, duration = 8, lengths = (1, 5, 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(0) * fact, spectrum(0) * fact, spectrum(0) * fact, spectrum(0) * fact),
          carrierFreq = (spectrum(0), spectrum(0), spectrum(0), spectrum(0)),
          modAmount = (100, 300, 2000, 200))
      .pan(panValue = (0.5f, 0.5f, -0.5f, -0.5f))
      .play    

    Note(startTime = startTime, duration = 8, lengths = (3, 4, 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(1) * fact, spectrum(1) * fact, spectrum(1) * fact, spectrum(1) * fact),
          carrierFreq = (spectrum(1), spectrum(1), spectrum(1), spectrum(1)),
          modAmount = (100, 2000, 500, 200))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play
  }

  /*
  Higher theme
  */
  def test9(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val (baseNote, octave) = (noteToHertz('c2), noteToHertz('fiss3))
    val fact = makeFact(baseNote, octave)
    val spectrum = makeSpectrum2(baseNote, fact, 50)
    
    val fact2 = spectrum(1) / spectrum.head
    val fact3 = spectrum(2) / spectrum.head
    val spectrum2 = makeSpectrum2(fact2, fact, 50)

    println(s"fact $fact fact2 $fact2 fact3 $fact3")
    println(s"spectrum $spectrum")
    println(s"spectrum2 $spectrum2")

    Note(startTime = startTime, duration = 8, lengths = (2, 5, 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(6) * fact, spectrum(6) * fact, spectrum(6) * fact, spectrum(6) * fact),
          carrierFreq = (spectrum(6), spectrum(6), spectrum(6), spectrum(6)),
          modAmount = (100, 300, 3000, 300))
      .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
      .play    


    Note(startTime = startTime, duration = 8, lengths = (1, 5, 2))
      .fm(ampValue = (0.5f, 0.5f),  
          modFreq = (spectrum(9) * fact, spectrum(9) * fact, spectrum(9) * fact, spectrum(9) * fact),
          carrierFreq = (spectrum(9), spectrum(9), spectrum(9), spectrum(9)),
          modAmount = (100, 2000, 500, 100))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play

    Note(startTime = startTime, duration = 8, lengths = (2, 5, 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(12) * fact, spectrum(12) * fact, spectrum(12) * fact, spectrum(12) * fact),
          carrierFreq = (spectrum(12), spectrum(12), spectrum(12), spectrum(12)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play  

    Note(startTime = startTime, duration = 8, lengths = (2, 5, 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(15) * fact, spectrum(15) * fact, spectrum(15) * fact, spectrum(15) * fact),
          carrierFreq = (spectrum(15), spectrum(15), spectrum(15), spectrum(15)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0f, 0f, 0f, 0f))
      .play 
  }

  /*
  Explore the lower theme
  */
  def test10(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val (baseNote, octave) = (noteToHertz('c2), noteToHertz('fiss3))
    val fact = makeFact(baseNote, octave)
    val spectrum = makeSpectrum2(baseNote, fact, 50)
    
    val fact2 = spectrum(1) / spectrum.head
    val fact3 = spectrum(2) / spectrum.head
    val spectrum2 = makeSpectrum2(fact2, fact, 50)

    Note(startTime = startTime, duration = 8, lengths = (1, 5, 2))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum2(3) * fact, spectrum2(3) * fact, spectrum2(3) * fact, spectrum2(3) * fact),
          carrierFreq = (spectrum2(3), spectrum2(3), spectrum2(3), spectrum2(3)),
          modAmount = (100, 200, 500, 300))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play 

    Note(startTime = startTime, duration = 8, lengths = (2, 5, 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum2(5) * fact, spectrum2(5) * fact, spectrum2(5) * fact, spectrum2(5) * fact),
          carrierFreq = (spectrum2(5), spectrum2(5), spectrum2(5), spectrum2(5)),
          modAmount = (200, 500, 300, 100))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play   

    Note(startTime = startTime, duration = 8, lengths = (2, 5, 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum2(7) * fact, spectrum2(7) * fact, spectrum2(7) * fact, spectrum2(7) * fact),
          carrierFreq = (spectrum2(7), spectrum2(7), spectrum2(7), spectrum2(7)),
          modAmount = (300, 200, 500, 100))
      .pan(panValue = (0f, 0f, 0f, 0f))
      .play
  }


  def test11(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val (baseNote, octave) = (noteToHertz('c2), noteToHertz('fiss3))
    val fact = makeFact(baseNote, octave)
    val spectrum = makeSpectrum2(baseNote, fact, 50)
    
    val fact2 = spectrum(1) / spectrum.head
    val fact3 = spectrum(2) / spectrum.head
    val spectrum2 = makeSpectrum2(fact2, fact, 50)

    /*
    simpleFm(0, 20f, 0.5f, 10f, fact, spectrum2(3), (spectrum(10), spectrum(35)), 10f, -0.5f)
    simpleFm(0, 20f, 0.5f, 10f, fact2, spectrum2(5), (spectrum(45), spectrum(15)), 10f, 0f)
    simpleFm(0, 20f, 0.5f, 10f, fact3, spectrum2(7), (spectrum(26), spectrum(46)), 10f, 0.5f)
    */

    Note(startTime = startTime, duration = 8, lengths = (2, 5, 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact),
          carrierFreq = (spectrum(3), spectrum(3), spectrum(3), spectrum(3)),
          modAmount = (100, 300, 3000, 300))
      .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
      .play    

    Note(startTime = startTime + 10, duration = 8, lengths = (2, 5, 1))
      .fm(ampValue = (0.5f, 0.5f), 
        modFreq = (spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact),
        carrierFreq = (spectrum(3), spectrum(3), spectrum(3), spectrum(3)),
        modAmount = (100, 300, 3000, 300))
      .ring(ringModFreq = (spectrum(7), spectrum(7), spectrum(7), spectrum(7)))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play
  }

  /*
  Test moog filter
  */
  def test12(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val (baseNote, octave) = (noteToHertz('c2), noteToHertz('fiss3))
    val fact = makeFact(baseNote, octave)
    val spectrum = makeSpectrum2(baseNote, fact, 50)
    
    val fact2 = spectrum(1) / spectrum.head
    val fact3 = spectrum(2) / spectrum.head
    val spectrum2 = makeSpectrum2(fact2, fact, 50)

    println(s"fact $fact fact2 $fact2 fact3 $fact3")
    println(s"spectrum $spectrum")
    println(s"spectrum2 $spectrum2")

    Note(startTime = startTime, duration = 8, lengths = (1, 5, 2))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq =   (spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact),
          carrierFreq = (spectrum(3), spectrum(3), spectrum(3), spectrum(3)),
          modAmount = (100, 300, 3000, 300)) 
       .lowPass(filterFreq = (2000, 2000, 2000, 2000))   
       .pan(panValue = (0f, 0f, 0f, 0f))
       .play

    Note(startTime = startTime + 10, duration = 8, lengths = (1, 5, 2))
      .fm(ampValue = (0.9f, 0.9f), 
          modFreq =   (spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact),
          carrierFreq = (spectrum(3), spectrum(3), spectrum(3), spectrum(3)),
          modAmount = (100, 300, 3000, 300))
       .highPass(filterFreq = (2000, 2000, 2000, 2000))   
       .pan(panValue = (0f, 0f, 0f, 0f))
       .play
  }

  /*
  Explore short pulses via slow pulseOsc. Filter to get pitch.
  Try to combine two slow pulses with fm and/or ring modulate. What
  kind of rythm do we get.

  Todo. Combine this with the ordinary theme. Filter them with a wide
  band pass filter.
  */
  def test13(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val (baseNote, octave) = (noteToHertz('c2), noteToHertz('fiss3))
    val fact = makeFact(baseNote, octave)
    val spectrum = makeSpectrum2(baseNote, fact, 50)
    
    val fact2 = spectrum.head /spectrum(1)
    val fact3 = spectrum.head / spectrum(2)
    val spectrum2 = makeSpectrum2(fact3, fact, 50)

    println(s"fact $fact fact2 $fact2 fact3 $fact3")
    println(s"spectrum2 $spectrum2")

    Note(startTime = startTime, duration = 13, lengths = (2, 8, 3))
      .pulse(ampValue = (0.5f, 0.5f), freq = (spectrum2(0), spectrum2(1), spectrum2(1), spectrum2(2)))
      .bandPass(lowerFreq = (1000, 1000, 4000, 4000), higherFreq = (1200, 1200, 6000, 6000))   
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play 

    Note(startTime = startTime, duration = 13, lengths = (3, 8, 2))
      .pulse(ampValue = (0.5f, 0.5f), freq = (spectrum2(3), spectrum2(2), spectrum2(2), spectrum2(1)))
      .bandPass(lowerFreq = (800, 800, 300, 300), higherFreq = (1000, 1000, 500, 500))   
      .pan(panValue = (0f, 0.5f, -0.5f, 0f))
      .play       
  }

  /*
  test to combine the original theme and the pulse theme. 
  */
  def test14(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val (baseNote, octave) = (noteToHertz('c2), noteToHertz('fiss3))
    val fact = makeFact(baseNote, octave)
    val spectrum = makeSpectrum2(baseNote, fact, 50)
    println(spectrum.zipWithIndex)

    val fact2 = spectrum.head /spectrum(1)
    val fact3 = spectrum.head / spectrum(2)
    val spectrum2 = makeSpectrum2(fact3, fact, 50)
    println(spectrum2.zipWithIndex)

    Note(startTime = startTime, duration = 8, lengths = (2, 5, 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq =   (spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact),
          carrierFreq = (spectrum(3), spectrum(3), spectrum(3), spectrum(3)),
          modAmount = (100, 300, 3000, 300))
       .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
       .play

       /*
       This doesn't really work. Maybe it should work better as a call and response?
       E.g you response to the main theme with this
       */
    Note(startTime = startTime, duration = 8, lengths = (2, 5, 1))
      .pulse(ampValue = (0.5f, 0.4f), freq = (spectrum2(3), spectrum2(7), spectrum2(5), spectrum2(2)))
      .bandPass(
        lowerFreq = (spectrum(15), spectrum(25), spectrum(21), spectrum(29)), 
        higherFreq = (spectrum(30), spectrum(33), spectrum(35), spectrum(33)))   
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play 

    Note(startTime = startTime, duration = 8, lengths = (1, 5, 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact),
          carrierFreq = (spectrum(4), spectrum(4), spectrum(4), spectrum(4)),
          modAmount = (100, 2000, 500, 100))   
       .pan(panValue = (-0.8f, 0, 0, 0.8f))
       .play  
       
    Note(startTime = startTime, duration = 8, lengths = (2, 5, 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact),
          carrierFreq = (spectrum(5), spectrum(5), spectrum(5), spectrum(5)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play
  }

  /*
  Test really slow pulse. Think Ryoanji

  Try to have a common slow delay (after the pan)
  */
  def test15(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    val (baseNote, octave) = (noteToHertz('c2), noteToHertz('fiss3))
    val fact = makeFact(baseNote, octave)
    val spectrum = makeSpectrum2(baseNote, fact, 50)
    println(s" ${spectrum.zipWithIndex}")

    val fact2 = spectrum.head /spectrum(1)
    val fact3 = spectrum.head / spectrum(2)
    val spectrum2 = makeSpectrum2(fact3, fact, 50)
    println(spectrum2.zipWithIndex)

    println(s"fact $fact fact2 $fact2 fact3 $fact3")
    println(s"spectrum2 ${spectrum2.zipWithIndex}")

    val delayAudioBus = staticAudioBus()
    val delayAmp = threeBlock(lengths = (10, 40, 10), vals = (0.001f, 0.05f, 0.03f, 0.001f))
    val delay = stereoDelay(delayAudioBus, delayAmp, delayTime = spectrum2(4), decayTime = spectrum2(9))
      .withNrOfChannels(2)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)
    delay.getOutputBus.staticBus(0)
    val graph = delay.buildGraph(startTime, 60, delay.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(startTime), graph)
     
    Note(startTime = startTime, duration = 60, lengths = (10, 40, 10))
      .pulse(ampValue = (0.2f, 0.3f), freq = (spectrum2(0) / 7, spectrum2(1) / 7, spectrum2(1) / 7, spectrum2(2) / 7))
      .bandPass(
        lowerFreq = (50, 80, 70, 90), 
        higherFreq = (110, 130, 120, 100))
  
      .pan(panValue = (-0.8f, 0, 0, 0.8f), output = Some(delayAudioBus))
      .play 

    Note(startTime = startTime, duration = 60, lengths = (10, 40, 10))
      .pulse(ampValue = (0.6f, 0.5f), freq = (spectrum2(0) / 7.02f, spectrum2(1) / 7.01f, spectrum2(1) / 7.02f, spectrum2(2) / 7.01f))
      .bandPass(
        lowerFreq = (1000, 1500, 1200, 1600), 
        higherFreq = (2000, 1700, 1800, 1900))   
      .pan(panValue = (0f, 0.5f, -0.5f, 0f), output = Some(delayAudioBus))
      .play       

    Note(startTime = startTime, duration = 60, lengths = (10, 40, 10))
      .pulse(ampValue = (0.5f, 0.6f), freq = (spectrum2(0) / 6.98f, spectrum2(1) / 6.99f, spectrum2(1) / 6.98f, spectrum2(2) / 6.99f))
      .bandPass(
        lowerFreq = (6000, 6500, 6400, 6700), 
        higherFreq = (7500, 6900, 7100, 7600))   
      .pan(panValue = (0.6f, -0.3f, 0.3f, -0.6f), output = Some(delayAudioBus))
      .play            
  }


  def main(args: Array[String]): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)

    
    test7(0f)
    test8(10f)
    test9(20f)
    test10(30f)
    
    //test11(0f)
    //test12(0f)
    //test14(0f)
    test15(0f)
    Console.println("Print q to quit")
    
    val cmd = StdIn.readLine()
    Console.println(s"You typed $cmd, goodBye")
    player.stopPlay()
    
  }
}
