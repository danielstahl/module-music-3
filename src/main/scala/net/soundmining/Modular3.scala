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
object Modular3 {

  def threeBlock(lengths: (Float, Float, Float), vals: (Float, Float, Float, Float)): ThreeBlockControl = {
    threeBlockcontrol(
      startValue1 =  vals._1, len1 = lengths._1, 
      startValue2 = vals._2, len2 = lengths._2, 
      startValue3 = vals._3, len3 = lengths._3, 
      endValue3 = vals._4, 
      Right(Instrument.LINEAR))

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

    def pulse(ampValue: (Float, Float), freq: (Float, Float, Float, Float), startValue: Float = 0.001f): Note = {
      val amp = threeBlock(lengths = lengths, vals = (startValue, ampValue._1, ampValue._2, 0.001f))
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

  /*
  Spectrum Vector((65.4064,0), (184.99724,1), (304.58807,2), (424.1789,3), (543.7698,4), (663.3606,5), (782.9514,6), (902.54224,7), (1022.1331,8), (1141.7239,9), (1261.3148,10), (1380.9056,11), (1500.4963,12), (1620.0873,13), (1739.6781,14), (1859.269,15), (1978.8597,16), (2098.4507,17), (2218.0415,18), (2337.6323,19), (2457.2231,20), (2576.814,21), (2696.4048,22), (2815.9956,23), (2935.5864,24), (3055.1775,25), (3174.7683,26), (3294.359,27), (3413.9497,28), (3533.5405,29), (3653.1316,30), (3772.7224,31), (3892.3132,32), (4011.904,33), (4131.4946,34), (4251.086,35), (4370.6763,36), (4490.2676,37), (4609.8584,38), (4729.449,39), (4849.04,40), (4968.6304,41), (5088.2217,42), (5207.8125,43), (5327.4033,44), (5446.994,45), (5566.585,46), (5686.176,47), (5805.766,48), (5925.3574,49))
  Spectrum 2 Vector((0.3535534,0), (1.0,1), (1.6464467,2), (2.2928932,3), (2.9393399,4), (3.5857866,5), (4.232233,6), (4.8786798,7), (5.5251265,8), (6.1715727,9), (6.81802,10), (7.464466,11), (8.110912,12), (8.7573595,13), (9.403806,14), (10.050253,15), (10.696699,16), (11.343146,17), (11.989593,18), (12.63604,19), (13.282487,20), (13.928933,21), (14.575379,22), (15.221826,23), (15.868272,24), (16.514719,25), (17.161165,26), (17.807613,27), (18.45406,28), (19.100506,29), (19.746952,30), (20.3934,31), (21.039846,32), (21.686293,33), (22.332739,34), (22.979187,35), (23.625631,36), (24.27208,37), (24.918528,38), (25.564972,39), (26.21142,40), (26.857864,41), (27.504313,42), (28.15076,43), (28.797205,44), (29.443653,45), (30.090097,46), (30.736546,47), (31.38299,48), (32.029438,49))
  Spectrum3 Vector((0.21473724,0), (0.6073686,1), (1.0,2), (1.3926313,3), (1.7852627,4), (2.177894,5), (2.5705254,6), (2.963157,7), (3.3557882,8), (3.7484195,9), (4.1410513,10), (4.5336823,11), (4.9263134,12), (5.3189454,13), (5.7115765,14), (6.104208,15), (6.496839,16), (6.8894706,17), (7.2821016,18), (7.6747336,19), (8.067365,20), (8.459996,21), (8.852628,22), (9.245258,23), (9.63789,24), (10.030522,25), (10.423153,26), (10.815784,27), (11.208416,28), (11.601047,29), (11.993679,30), (12.38631,31), (12.778941,32), (13.171573,33), (13.564204,34), (13.956836,35), (14.349466,36), (14.742098,37), (15.13473,38), (15.527361,39), (15.919992,40), (16.312624,41), (16.705256,42), (17.097887,43), (17.490519,44), (17.88315,45), (18.27578,46), (18.668411,47), (19.061043,48), (19.453674,49))
  */
  def exposition1(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    println(s"Exposition1 at startTime $startTime ")
    val spectrum = spectrums.head
    val fact = facts.head
    val fact2 = spectrum.head /spectrum(1)
    val spectrum2 = makeSpectrum2(fact2, fact, 50)
    val fact3 = spectrum.head / spectrum(2)
    val spectrum3 = makeSpectrum2(fact3, fact, 50)

    println(s"Spectrum ${spectrum.zipWithIndex}")
    println(s"Spectrum2 ${spectrum2.zipWithIndex}")
    println(s"Spectrum3 ${spectrum3.zipWithIndex}")
    println(s"The pulse? ${spectrum3.head / 7}")

    val duration = spectrum.head // 65.4064
    val pulseDurationDivision = duration / 34
    val pulseLengths = (pulseDurationDivision, pulseDurationDivision * 32, pulseDurationDivision)

    val delayAudioBus = staticAudioBus()
    val delayAmp = threeBlock(lengths = pulseLengths, vals = (0.02f, 0.05f, 0.03f, 0.001f))
    val delay = stereoDelay(delayAudioBus, delayAmp, delayTime = spectrum3(5), decayTime = spectrum3(10))
      .withNrOfChannels(2)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)
    delay.getOutputBus.staticBus(0)
    val graph = delay.buildGraph(startTime, duration, delay.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(startTime), graph)

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.1f, 0.2f), freq = (spectrum3(0) / 11, spectrum3(1) / 11, spectrum3(1) / 11, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(0), spectrum(0), spectrum(0), spectrum(0)),
        higherFreq = (spectrum(1), spectrum(1), spectrum(1), spectrum(1)))
      .ring(ringModFreq = (spectrum(2), spectrum(2), spectrum(2), spectrum(2)))  
      .pan(panValue = (-0.8f, 0, 0, 0.8f), output = Some(delayAudioBus))
      .play 

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.6f, 0.5f), freq = (spectrum3(0) / 11, (spectrum3(1) / 11) * 1.004f, (spectrum3(1) / 11) * 0.997f, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(8), spectrum(9), spectrum(9), spectrum(8)), 
        higherFreq = (spectrum(17), spectrum(14), spectrum(14), spectrum(17)))
      .ring(ringModFreq = (spectrum(11), spectrum(11), spectrum(11), spectrum(11)))  
      .pan(panValue = (0f, 0.5f, -0.5f, 0f), output = Some(delayAudioBus))
      .play       

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.8f, 0.9f), freq = (spectrum3(0) / 11, spectrum3(1) / 11 * 0.997f, (spectrum3(1) / 11) * 1.004f, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(38), spectrum(39), spectrum(39), spectrum(38)), 
        higherFreq = (spectrum(49), spectrum(48), spectrum(48), spectrum(49)))
      .ring(ringModFreq = (spectrum(42), spectrum(42), spectrum(42), spectrum(42)))    
      .pan(panValue = (0.6f, -0.3f, 0.3f, -0.6f), output = Some(delayAudioBus))
      .play 

      
    // Middle theme
    val longDurationDivision = duration / 4
    val longStartTimes = absolute(startTime + longDurationDivision, Seq(longDurationDivision, longDurationDivision, longDurationDivision))

    val middleThemeDuration = spectrum3(20)
    val middleThemePulse = middleThemeDuration / 8

    println(s"long start times $longStartTimes")

    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse *1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq =   (spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact),
          carrierFreq = (spectrum(3), spectrum(3), spectrum(3), spectrum(3)),
          modAmount = (100, 300, 3000, 300))
       .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
       .play

    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact),
          carrierFreq = (spectrum(4), spectrum(4), spectrum(4), spectrum(4)),
          modAmount = (100, 2000, 500, 100))   
       .pan(panValue = (-0.8f, 0, 0, 0.8f))
       .play  
       
    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact),
          carrierFreq = (spectrum(5), spectrum(5), spectrum(5), spectrum(5)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play  
      
  
    // Higher theme
    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(6) * fact, spectrum(6) * fact, spectrum(6) * fact, spectrum(6) * fact),
          carrierFreq = (spectrum(6), spectrum(6), spectrum(6), spectrum(6)),
          modAmount = (100, 300, 3000, 300))
      .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
      .play    

    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),  
          modFreq = (spectrum(9) * fact, spectrum(9) * fact, spectrum(9) * fact, spectrum(9) * fact),
          carrierFreq = (spectrum(9), spectrum(9), spectrum(9), spectrum(9)),
          modAmount = (100, 2000, 500, 100))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play

    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(12) * fact, spectrum(12) * fact, spectrum(12) * fact, spectrum(12) * fact),
          carrierFreq = (spectrum(12), spectrum(12), spectrum(12), spectrum(12)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play  

    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(15) * fact, spectrum(15) * fact, spectrum(15) * fact, spectrum(15) * fact),
          carrierFreq = (spectrum(15), spectrum(15), spectrum(15), spectrum(15)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0f, 0f, 0f, 0f))
      .play 

    // Lower theme
    Note(startTime = longStartTimes(2), duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(0) * fact, spectrum(0) * fact, spectrum(0) * fact, spectrum(0) * fact),
          carrierFreq = (spectrum(0), spectrum(0), spectrum(0), spectrum(0)),
          modAmount = (100, 300, 2000, 200))
      .pan(panValue = (0.5f, 0.5f, -0.5f, -0.5f))
      .play    

    Note(startTime = longStartTimes(2), duration = middleThemeDuration, lengths = (middleThemePulse * 3, middleThemePulse * 4, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(1) * fact, spectrum(1) * fact, spectrum(1) * fact, spectrum(1) * fact),
          carrierFreq = (spectrum(1), spectrum(1), spectrum(1), spectrum(1)),
          modAmount = (100, 2000, 500, 200))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play 
  }

  def exposition2(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    println(s"Exposition2 at startTime $startTime ")
    val spectrum = spectrums(1)
    val fact = facts(1)
    val fact2 = spectrum.head /spectrum(1)
    val spectrum2 = makeSpectrum2(fact2, fact, 50)
    val fact3 = spectrum.head / spectrum(2)
    val spectrum3 = makeSpectrum2(fact3, fact, 50)

    println(s"Spectrum ${spectrum.zipWithIndex}")
    println(s"Spectrum2 ${spectrum2.zipWithIndex}")
    println(s"Spectrum3 ${spectrum3.zipWithIndex}")
    println(s"The pulse? ${spectrum3.head / 7}")

    val duration = spectrum.head // 77.78175
    val pulseDurationDivision = duration / 34
    val pulseLengths = (pulseDurationDivision, pulseDurationDivision * 32, pulseDurationDivision)

    val delayAudioBus = staticAudioBus()
    val delayAmp = threeBlock(lengths = pulseLengths, vals = (0.02f, 0.05f, 0.03f, 0.001f))
    val delay = stereoDelay(delayAudioBus, delayAmp, delayTime = spectrum3(5), decayTime = spectrum3(10))
      .withNrOfChannels(2)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)
    delay.getOutputBus.staticBus(0)
    val graph = delay.buildGraph(startTime, duration, delay.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(startTime), graph)

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.1f, 0.2f), freq = (spectrum3(0) / 11, spectrum3(1) / 11, spectrum3(1) / 11, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(0), spectrum(0), spectrum(0), spectrum(0)),
        higherFreq = (spectrum(1), spectrum(1), spectrum(1), spectrum(1)))
      .ring(ringModFreq = (spectrum(2), spectrum(2), spectrum(2), spectrum(2)))    
      .pan(panValue = (-0.8f, 0, 0, 0.8f), output = Some(delayAudioBus))
      .play 

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.6f, 0.5f), freq = (spectrum3(0) / 11, (spectrum3(1) / 11) * 1.004f, (spectrum3(1) / 11) * 0.997f, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(8), spectrum(9), spectrum(9), spectrum(8)), 
        higherFreq = (spectrum(17), spectrum(14), spectrum(14), spectrum(17))) 
      .ring(ringModFreq = (spectrum(11), spectrum(11), spectrum(11), spectrum(11)))      
      .pan(panValue = (0f, 0.5f, -0.5f, 0f), output = Some(delayAudioBus))
      .play       

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.8f, 0.9f), freq = (spectrum3(0) / 11, spectrum3(1) / 11 * 0.997f, (spectrum3(1) / 11) * 1.004f, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(38), spectrum(39), spectrum(39), spectrum(38)), 
        higherFreq = (spectrum(49), spectrum(48), spectrum(48), spectrum(49))) 
      .ring(ringModFreq = (spectrum(42), spectrum(42), spectrum(42), spectrum(42)))      
      .pan(panValue = (0.6f, -0.3f, 0.3f, -0.6f), output = Some(delayAudioBus))
      .play 

    // Middle theme
    val longDurationDivision = duration / 4
    val longStartTimes = absolute(startTime + longDurationDivision, Seq(longDurationDivision, longDurationDivision, longDurationDivision))

    val middleThemeDuration = spectrum3(20)
    val middleThemePulse = middleThemeDuration / 8

    println(s"long start times $longStartTimes")

    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse *1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq =   (spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact),
          carrierFreq = (spectrum(3), spectrum(3), spectrum(3), spectrum(3)),
          modAmount = (100, 300, 3000, 300))
       .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
       .play

    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact),
          carrierFreq = (spectrum(4), spectrum(4), spectrum(4), spectrum(4)),
          modAmount = (100, 2000, 500, 100))   
       .pan(panValue = (-0.8f, 0, 0, 0.8f))
       .play  
       
    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact),
          carrierFreq = (spectrum(5), spectrum(5), spectrum(5), spectrum(5)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play  
      
  
    // Higher theme
    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(6) * fact, spectrum(6) * fact, spectrum(6) * fact, spectrum(6) * fact),
          carrierFreq = (spectrum(6), spectrum(6), spectrum(6), spectrum(6)),
          modAmount = (100, 300, 3000, 300))
      .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
      .play    


    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),  
          modFreq = (spectrum(9) * fact, spectrum(9) * fact, spectrum(9) * fact, spectrum(9) * fact),
          carrierFreq = (spectrum(9), spectrum(9), spectrum(9), spectrum(9)),
          modAmount = (100, 2000, 500, 100))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play

    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(12) * fact, spectrum(12) * fact, spectrum(12) * fact, spectrum(12) * fact),
          carrierFreq = (spectrum(12), spectrum(12), spectrum(12), spectrum(12)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play  

    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(15) * fact, spectrum(15) * fact, spectrum(15) * fact, spectrum(15) * fact),
          carrierFreq = (spectrum(15), spectrum(15), spectrum(15), spectrum(15)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0f, 0f, 0f, 0f))
      .play 

    // Lower theme
    Note(startTime = longStartTimes(2), duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(0) * fact, spectrum(0) * fact, spectrum(0) * fact, spectrum(0) * fact),
          carrierFreq = (spectrum(0), spectrum(0), spectrum(0), spectrum(0)),
          modAmount = (100, 300, 2000, 200))
      .pan(panValue = (0.5f, 0.5f, -0.5f, -0.5f))
      .play    

    Note(startTime = longStartTimes(2), duration = middleThemeDuration, lengths = (middleThemePulse * 3, middleThemePulse * 4, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(1) * fact, spectrum(1) * fact, spectrum(1) * fact, spectrum(1) * fact),
          carrierFreq = (spectrum(1), spectrum(1), spectrum(1), spectrum(1)),
          modAmount = (100, 2000, 500, 200))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play 
  }

  def exposition3(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    println(s"Exposition3 at startTime $startTime ")
    val spectrum = spectrums(2)
    val fact = facts(2)
    val fact2 = spectrum.head /spectrum(1)
    val spectrum2 = makeSpectrum2(fact2, fact, 50)
    val fact3 = spectrum.head / spectrum(2)
    val spectrum3 = makeSpectrum2(fact3, fact, 50)

    println(s"Spectrum ${spectrum.zipWithIndex}")
    println(s"Spectrum2 ${spectrum2.zipWithIndex}")
    println(s"Spectrum3 ${spectrum3.zipWithIndex}")
    println(s"The pulse? ${spectrum3.head / 7}")

    val duration = spectrum.head // 116.540955
    val pulseDurationDivision = duration / 34
    val pulseLengths = (pulseDurationDivision, pulseDurationDivision * 32, pulseDurationDivision)

    val delayAudioBus = staticAudioBus()
    val delayAmp = threeBlock(lengths = pulseLengths, vals = (0.02f, 0.05f, 0.03f, 0.001f))
    val delay = stereoDelay(delayAudioBus, delayAmp, delayTime = spectrum3(5), decayTime = spectrum3(10))
      .withNrOfChannels(2)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)
    delay.getOutputBus.staticBus(0)
    val graph = delay.buildGraph(startTime, duration, delay.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(startTime), graph)

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.1f, 0.2f), freq = (spectrum3(0) / 11, spectrum3(1) / 11, spectrum3(1) / 11, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(0), spectrum(0), spectrum(0), spectrum(0)),
        higherFreq = (spectrum(1), spectrum(1), spectrum(1), spectrum(1)))
      .ring(ringModFreq = (spectrum(2), spectrum(2), spectrum(2), spectrum(2)))     
      .pan(panValue = (-0.8f, 0, 0, 0.8f), output = Some(delayAudioBus))
      .play 

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.6f, 0.5f), freq = (spectrum3(0) / 11, (spectrum3(1) / 11) * 1.004f, (spectrum3(1) / 11) * 0.997f, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(8), spectrum(9), spectrum(9), spectrum(8)), 
        higherFreq = (spectrum(17), spectrum(14), spectrum(14), spectrum(17)))   
      .ring(ringModFreq = (spectrum(11), spectrum(11), spectrum(11), spectrum(11)))    
      .pan(panValue = (0f, 0.5f, -0.5f, 0f), output = Some(delayAudioBus))
      .play       

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.8f, 0.9f), freq = (spectrum3(0) / 11, spectrum3(1) / 11 * 0.997f, (spectrum3(1) / 11) * 1.004f, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(38), spectrum(39), spectrum(39), spectrum(38)), 
        higherFreq = (spectrum(49), spectrum(48), spectrum(48), spectrum(49)))   
      .ring(ringModFreq = (spectrum(42), spectrum(42), spectrum(42), spectrum(42)))    
      .pan(panValue = (0.6f, -0.3f, 0.3f, -0.6f), output = Some(delayAudioBus))
      .play 

    // Middle theme
    val longDurationDivision = duration / 4
    val longStartTimes = absolute(startTime + longDurationDivision, Seq(longDurationDivision, longDurationDivision, longDurationDivision))

    val middleThemeDuration = spectrum3(20)
    val middleThemePulse = middleThemeDuration / 8

    println(s"long start times $longStartTimes")

    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse *1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq =   (spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact),
          carrierFreq = (spectrum(3), spectrum(3), spectrum(3), spectrum(3)),
          modAmount = (100, 300, 3000, 300))
       .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
       .play

    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact),
          carrierFreq = (spectrum(4), spectrum(4), spectrum(4), spectrum(4)),
          modAmount = (100, 2000, 500, 100))   
       .pan(panValue = (-0.8f, 0, 0, 0.8f))
       .play  
       
    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact),
          carrierFreq = (spectrum(5), spectrum(5), spectrum(5), spectrum(5)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play  
      
  
    // Higher theme
    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(6) * fact, spectrum(6) * fact, spectrum(6) * fact, spectrum(6) * fact),
          carrierFreq = (spectrum(6), spectrum(6), spectrum(6), spectrum(6)),
          modAmount = (100, 300, 3000, 300))
      .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
      .play    


    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),  
          modFreq = (spectrum(9) * fact, spectrum(9) * fact, spectrum(9) * fact, spectrum(9) * fact),
          carrierFreq = (spectrum(9), spectrum(9), spectrum(9), spectrum(9)),
          modAmount = (100, 2000, 500, 100))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play

    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(12) * fact, spectrum(12) * fact, spectrum(12) * fact, spectrum(12) * fact),
          carrierFreq = (spectrum(12), spectrum(12), spectrum(12), spectrum(12)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play  

    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(15) * fact, spectrum(15) * fact, spectrum(15) * fact, spectrum(15) * fact),
          carrierFreq = (spectrum(15), spectrum(15), spectrum(15), spectrum(15)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0f, 0f, 0f, 0f))
      .play 

    // Lower theme
    Note(startTime = longStartTimes(2), duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(0) * fact, spectrum(0) * fact, spectrum(0) * fact, spectrum(0) * fact),
          carrierFreq = (spectrum(0), spectrum(0), spectrum(0), spectrum(0)),
          modAmount = (100, 300, 2000, 200))
      .pan(panValue = (0.5f, 0.5f, -0.5f, -0.5f))
      .play    

    Note(startTime = longStartTimes(2), duration = middleThemeDuration, lengths = (middleThemePulse * 3, middleThemePulse * 4, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(1) * fact, spectrum(1) * fact, spectrum(1) * fact, spectrum(1) * fact),
          carrierFreq = (spectrum(1), spectrum(1), spectrum(1), spectrum(1)),
          modAmount = (100, 2000, 500, 200))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play 
  }


  def exposition4(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    println(s"Exposition4 at startTime $startTime ")
    val spectrum = spectrums(3)
    val fact = facts(3)
    val fact2 = spectrum.head /spectrum(1)
    val spectrum2 = makeSpectrum2(fact2, fact, 50)
    val fact3 = spectrum.head / spectrum(2)
    val spectrum3 = makeSpectrum2(fact3, fact, 50)

    println(s"Spectrum ${spectrum.zipWithIndex}")
    println(s"Spectrum2 ${spectrum2.zipWithIndex}")
    println(s"Spectrum3 ${spectrum3.zipWithIndex}")
    println(s"The pulse? ${spectrum3.head / 7}")

    val duration = spectrum.head // 69.29566 
    val pulseDurationDivision = duration / 34
    val pulseLengths = (pulseDurationDivision, pulseDurationDivision * 32, pulseDurationDivision)

    val delayAudioBus = staticAudioBus()
    val delayAmp = threeBlock(lengths = pulseLengths, vals = (0.02f, 0.05f, 0.03f, 0.001f))
    val delay = stereoDelay(delayAudioBus, delayAmp, delayTime = spectrum3(5), decayTime = spectrum3(10))
      .withNrOfChannels(2)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)
    delay.getOutputBus.staticBus(0)
    val graph = delay.buildGraph(startTime, duration, delay.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(startTime), graph)

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.1f, 0.2f), freq = (spectrum3(0) / 11, spectrum3(1) / 11, spectrum3(1) / 11, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(0), spectrum(0), spectrum(0), spectrum(0)),
        higherFreq = (spectrum(1), spectrum(1), spectrum(1), spectrum(1)))
      .ring(ringModFreq = (spectrum(2), spectrum(2), spectrum(2), spectrum(2)))     
      .pan(panValue = (-0.8f, 0, 0, 0.8f), output = Some(delayAudioBus))
      .play 

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.6f, 0.5f), freq = (spectrum3(0) / 11, (spectrum3(1) / 11) * 1.004f, (spectrum3(1) / 11) * 0.997f, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(8), spectrum(9), spectrum(9), spectrum(8)), 
        higherFreq = (spectrum(17), spectrum(14), spectrum(14), spectrum(17)))   
      .ring(ringModFreq = (spectrum(11), spectrum(11), spectrum(11), spectrum(11)))    
      .pan(panValue = (0f, 0.5f, -0.5f, 0f), output = Some(delayAudioBus))
      .play       

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.8f, 0.9f), freq = (spectrum3(0) / 11, spectrum3(1) / 11 * 0.997f, (spectrum3(1) / 11) * 1.004f, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(38), spectrum(39), spectrum(39), spectrum(38)), 
        higherFreq = (spectrum(49), spectrum(48), spectrum(48), spectrum(49)))   
      .ring(ringModFreq = (spectrum(42), spectrum(42), spectrum(42), spectrum(42)))    
      .pan(panValue = (0.6f, -0.3f, 0.3f, -0.6f), output = Some(delayAudioBus))
      .play 

    // Middle theme
    val longDurationDivision = duration / 4
    val longStartTimes = absolute(startTime + longDurationDivision, Seq(longDurationDivision, longDurationDivision, longDurationDivision))

    val middleThemeDuration = spectrum3(20)
    val middleThemePulse = middleThemeDuration / 8

    println(s"long start times $longStartTimes")
    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse *1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq =   (spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact),
          carrierFreq = (spectrum(3), spectrum(3), spectrum(3), spectrum(3)),
          modAmount = (100, 300, 3000, 300))
       .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
       .play

    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact),
          carrierFreq = (spectrum(4), spectrum(4), spectrum(4), spectrum(4)),
          modAmount = (100, 2000, 500, 100))   
       .pan(panValue = (-0.8f, 0, 0, 0.8f))
       .play  
       
    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact),
          carrierFreq = (spectrum(5), spectrum(5), spectrum(5), spectrum(5)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play  
      
  
    // Higher theme
    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(6) * fact, spectrum(6) * fact, spectrum(6) * fact, spectrum(6) * fact),
          carrierFreq = (spectrum(6), spectrum(6), spectrum(6), spectrum(6)),
          modAmount = (100, 300, 3000, 300))
      .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
      .play    


    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),  
          modFreq = (spectrum(9) * fact, spectrum(9) * fact, spectrum(9) * fact, spectrum(9) * fact),
          carrierFreq = (spectrum(9), spectrum(9), spectrum(9), spectrum(9)),
          modAmount = (100, 2000, 500, 100))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play

    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(12) * fact, spectrum(12) * fact, spectrum(12) * fact, spectrum(12) * fact),
          carrierFreq = (spectrum(12), spectrum(12), spectrum(12), spectrum(12)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play  

    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(15) * fact, spectrum(15) * fact, spectrum(15) * fact, spectrum(15) * fact),
          carrierFreq = (spectrum(15), spectrum(15), spectrum(15), spectrum(15)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0f, 0f, 0f, 0f))
      .play 

    // Lower theme
    Note(startTime = longStartTimes(2), duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(0) * fact, spectrum(0) * fact, spectrum(0) * fact, spectrum(0) * fact),
          carrierFreq = (spectrum(0), spectrum(0), spectrum(0), spectrum(0)),
          modAmount = (100, 300, 2000, 200))
      .pan(panValue = (0.5f, 0.5f, -0.5f, -0.5f))
      .play    

    Note(startTime = longStartTimes(2), duration = middleThemeDuration, lengths = (middleThemePulse * 3, middleThemePulse * 4, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(1) * fact, spectrum(1) * fact, spectrum(1) * fact, spectrum(1) * fact),
          carrierFreq = (spectrum(1), spectrum(1), spectrum(1), spectrum(1)),
          modAmount = (100, 2000, 500, 200))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play 
  }


  def exposition5(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    println(s"Exposition5 at startTime $startTime ")
    val spectrum = spectrums(4)
    val fact = facts(4)
    val fact2 = spectrum.head /spectrum(1)
    val spectrum2 = makeSpectrum2(fact2, fact, 50)
    val fact3 = spectrum.head / spectrum(2)
    val spectrum3 = makeSpectrum2(fact3, fact, 50)

    println(s"Spectrum ${spectrum.zipWithIndex}")
    println(s"Spectrum2 ${spectrum2.zipWithIndex}")
    println(s"Spectrum3 ${spectrum3.zipWithIndex}")
    println(s"The pulse? ${spectrum3.head / 7}")

    val duration = spectrum.head // 61.735424
    val pulseDurationDivision = duration / 34
    val pulseLengths = (pulseDurationDivision, pulseDurationDivision * 32, pulseDurationDivision)

    val delayAudioBus = staticAudioBus()
    val delayAmp = threeBlock(lengths = pulseLengths, vals = (0.02f, 0.05f, 0.03f, 0.001f))
    val delay = stereoDelay(delayAudioBus, delayAmp, delayTime = spectrum3(5), decayTime = spectrum3(10))
      .withNrOfChannels(2)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)
    delay.getOutputBus.staticBus(0)
    val graph = delay.buildGraph(startTime, duration, delay.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(startTime), graph)

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.1f, 0.2f), freq = (spectrum3(0) / 11, spectrum3(1) / 11, spectrum3(1) / 11, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(0), spectrum(0), spectrum(0), spectrum(0)),
        higherFreq = (spectrum(1), spectrum(1), spectrum(1), spectrum(1)))
      .ring(ringModFreq = (spectrum(2), spectrum(2), spectrum(2), spectrum(2)))     
      .pan(panValue = (-0.8f, 0, 0, 0.8f), output = Some(delayAudioBus))
      .play 

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.6f, 0.5f), freq = (spectrum3(0) / 11, (spectrum3(1) / 11) * 1.004f, (spectrum3(1) / 11) * 0.997f, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(8), spectrum(9), spectrum(9), spectrum(8)), 
        higherFreq = (spectrum(17), spectrum(14), spectrum(14), spectrum(17)))   
      .ring(ringModFreq = (spectrum(11), spectrum(11), spectrum(11), spectrum(11)))    
      .pan(panValue = (0f, 0.5f, -0.5f, 0f), output = Some(delayAudioBus))
      .play       

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.8f, 0.9f), freq = (spectrum3(0) / 11, spectrum3(1) / 11 * 0.997f, (spectrum3(1) / 11) * 1.004f, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(38), spectrum(39), spectrum(39), spectrum(38)), 
        higherFreq = (spectrum(49), spectrum(48), spectrum(48), spectrum(49)))   
      .ring(ringModFreq = (spectrum(42), spectrum(42), spectrum(42), spectrum(42)))    
      .pan(panValue = (0.6f, -0.3f, 0.3f, -0.6f), output = Some(delayAudioBus))
      .play 

    // Middle theme
    val longDurationDivision = duration / 4
    val longStartTimes = absolute(startTime + longDurationDivision, Seq(longDurationDivision, longDurationDivision, longDurationDivision))

    val middleThemeDuration = spectrum3(20)
    val middleThemePulse = middleThemeDuration / 8

    println(s"long start times $longStartTimes")
    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse *1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq =   (spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact),
          carrierFreq = (spectrum(3), spectrum(3), spectrum(3), spectrum(3)),
          modAmount = (100, 300, 3000, 300))
       .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
       .play

    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact),
          carrierFreq = (spectrum(4), spectrum(4), spectrum(4), spectrum(4)),
          modAmount = (100, 2000, 500, 100))   
       .pan(panValue = (-0.8f, 0, 0, 0.8f))
       .play  
       
    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact),
          carrierFreq = (spectrum(5), spectrum(5), spectrum(5), spectrum(5)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play  
      
  
    // Higher theme
    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(6) * fact, spectrum(6) * fact, spectrum(6) * fact, spectrum(6) * fact),
          carrierFreq = (spectrum(6), spectrum(6), spectrum(6), spectrum(6)),
          modAmount = (100, 300, 3000, 300))
      .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
      .play    


    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),  
          modFreq = (spectrum(9) * fact, spectrum(9) * fact, spectrum(9) * fact, spectrum(9) * fact),
          carrierFreq = (spectrum(9), spectrum(9), spectrum(9), spectrum(9)),
          modAmount = (100, 2000, 500, 100))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play

    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(12) * fact, spectrum(12) * fact, spectrum(12) * fact, spectrum(12) * fact),
          carrierFreq = (spectrum(12), spectrum(12), spectrum(12), spectrum(12)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play  

    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(15) * fact, spectrum(15) * fact, spectrum(15) * fact, spectrum(15) * fact),
          carrierFreq = (spectrum(15), spectrum(15), spectrum(15), spectrum(15)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0f, 0f, 0f, 0f))
      .play 

    // Lower theme
    Note(startTime = longStartTimes(2), duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(0) * fact, spectrum(0) * fact, spectrum(0) * fact, spectrum(0) * fact),
          carrierFreq = (spectrum(0), spectrum(0), spectrum(0), spectrum(0)),
          modAmount = (100, 300, 2000, 200))
      .pan(panValue = (0.5f, 0.5f, -0.5f, -0.5f))
      .play    

    Note(startTime = longStartTimes(2), duration = middleThemeDuration, lengths = (middleThemePulse * 3, middleThemePulse * 4, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(1) * fact, spectrum(1) * fact, spectrum(1) * fact, spectrum(1) * fact),
          carrierFreq = (spectrum(1), spectrum(1), spectrum(1), spectrum(1)),
          modAmount = (100, 2000, 500, 200))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play 
  }


  def exposition6(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    println(s"Exposition6 at startTime $startTime ")
    val spectrum = spectrums(5)
    val fact = facts(5)
    val fact2 = spectrum.head /spectrum(1)
    val spectrum2 = makeSpectrum2(fact2, fact, 50)
    val fact3 = spectrum.head / spectrum(2)
    val spectrum3 = makeSpectrum2(fact3, fact, 50)

    println(s"Spectrum ${spectrum.zipWithIndex}")
    println(s"Spectrum2 ${spectrum2.zipWithIndex}")
    println(s"Spectrum3 ${spectrum3.zipWithIndex}")
    println(s"The pulse? ${spectrum3.head / 7}")

    val duration = spectrum.head // 77.78175
    val pulseDurationDivision = duration / 34
    val pulseLengths = (pulseDurationDivision, pulseDurationDivision * 32, pulseDurationDivision)

    val delayAudioBus = staticAudioBus()
    val delayAmp = threeBlock(lengths = pulseLengths, vals = (0.02f, 0.05f, 0.03f, 0.001f))
    val delay = stereoDelay(delayAudioBus, delayAmp, delayTime = spectrum3(5), decayTime = spectrum3(10))
      .withNrOfChannels(2)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)
    delay.getOutputBus.staticBus(0)
    val graph = delay.buildGraph(startTime, duration, delay.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(startTime), graph)

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.1f, 0.2f), freq = (spectrum3(0) / 11, spectrum3(1) / 11, spectrum3(1) / 11, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(0), spectrum(0), spectrum(0), spectrum(0)),
        higherFreq = (spectrum(1), spectrum(1), spectrum(1), spectrum(1)))
      .ring(ringModFreq = (spectrum(2), spectrum(2), spectrum(2), spectrum(2)))     
      .pan(panValue = (-0.8f, 0, 0, 0.8f), output = Some(delayAudioBus))
      .play 

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.6f, 0.5f), freq = (spectrum3(0) / 11, (spectrum3(1) / 11) * 1.004f, (spectrum3(1) / 11) * 0.997f, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(8), spectrum(9), spectrum(9), spectrum(8)), 
        higherFreq = (spectrum(17), spectrum(14), spectrum(14), spectrum(17)))   
      .ring(ringModFreq = (spectrum(11), spectrum(11), spectrum(11), spectrum(11)))    
      .pan(panValue = (0f, 0.5f, -0.5f, 0f), output = Some(delayAudioBus))
      .play       

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.8f, 0.9f), freq = (spectrum3(0) / 11, spectrum3(1) / 11 * 0.997f, (spectrum3(1) / 11) * 1.004f, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(38), spectrum(39), spectrum(39), spectrum(38)), 
        higherFreq = (spectrum(49), spectrum(48), spectrum(48), spectrum(49)))   
      .ring(ringModFreq = (spectrum(42), spectrum(42), spectrum(42), spectrum(42)))    
      .pan(panValue = (0.6f, -0.3f, 0.3f, -0.6f), output = Some(delayAudioBus))
      .play 

    // Middle theme
    val longDurationDivision = duration / 4
    val longStartTimes = absolute(startTime + longDurationDivision, Seq(longDurationDivision, longDurationDivision, longDurationDivision))

    val middleThemeDuration = spectrum3(20)
    val middleThemePulse = middleThemeDuration / 8

    println(s"long start times $longStartTimes")
    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse *1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq =   (spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact),
          carrierFreq = (spectrum(3), spectrum(3), spectrum(3), spectrum(3)),
          modAmount = (100, 300, 3000, 300))
       .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
       .play

    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact),
          carrierFreq = (spectrum(4), spectrum(4), spectrum(4), spectrum(4)),
          modAmount = (100, 2000, 500, 100))   
       .pan(panValue = (-0.8f, 0, 0, 0.8f))
       .play  
       
    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact),
          carrierFreq = (spectrum(5), spectrum(5), spectrum(5), spectrum(5)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play  
      
  
    // Higher theme
    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(6) * fact, spectrum(6) * fact, spectrum(6) * fact, spectrum(6) * fact),
          carrierFreq = (spectrum(6), spectrum(6), spectrum(6), spectrum(6)),
          modAmount = (100, 300, 3000, 300))
      .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
      .play    


    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),  
          modFreq = (spectrum(9) * fact, spectrum(9) * fact, spectrum(9) * fact, spectrum(9) * fact),
          carrierFreq = (spectrum(9), spectrum(9), spectrum(9), spectrum(9)),
          modAmount = (100, 2000, 500, 100))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play

    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(12) * fact, spectrum(12) * fact, spectrum(12) * fact, spectrum(12) * fact),
          carrierFreq = (spectrum(12), spectrum(12), spectrum(12), spectrum(12)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play  

    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(15) * fact, spectrum(15) * fact, spectrum(15) * fact, spectrum(15) * fact),
          carrierFreq = (spectrum(15), spectrum(15), spectrum(15), spectrum(15)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0f, 0f, 0f, 0f))
      .play 

    // Lower theme
    Note(startTime = longStartTimes(2), duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(0) * fact, spectrum(0) * fact, spectrum(0) * fact, spectrum(0) * fact),
          carrierFreq = (spectrum(0), spectrum(0), spectrum(0), spectrum(0)),
          modAmount = (100, 300, 2000, 200))
      .pan(panValue = (0.5f, 0.5f, -0.5f, -0.5f))
      .play    

    Note(startTime = longStartTimes(2), duration = middleThemeDuration, lengths = (middleThemePulse * 3, middleThemePulse * 4, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(1) * fact, spectrum(1) * fact, spectrum(1) * fact, spectrum(1) * fact),
          carrierFreq = (spectrum(1), spectrum(1), spectrum(1), spectrum(1)),
          modAmount = (100, 2000, 500, 200))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play 
  }

  def exposition7(startTime: Float = 0f)(implicit player: MusicPlayer): Unit = {
    println(s"Exposition7 at startTime $startTime ")
    val spectrum = spectrums(6)
    val fact = facts(6)
    val fact2 = spectrum.head /spectrum(1)
    val spectrum2 = makeSpectrum2(fact2, fact, 50)
    val fact3 = spectrum.head / spectrum(2)
    val spectrum3 = makeSpectrum2(fact3, fact, 50)

    println(s"Spectrum ${spectrum.zipWithIndex}")
    println(s"Spectrum2 ${spectrum2.zipWithIndex}")
    println(s"Spectrum3 ${spectrum3.zipWithIndex}")
    println(s"The pulse? ${spectrum3.head / 7}")

    val duration = spectrum.head // 
    val pulseDurationDivision = duration / 34
    val pulseLengths = (pulseDurationDivision, pulseDurationDivision * 32, pulseDurationDivision)

    val delayAudioBus = staticAudioBus()
    val delayAmp = threeBlock(lengths = pulseLengths, vals = (0.02f, 0.05f, 0.03f, 0.001f))
    val delay = stereoDelay(delayAudioBus, delayAmp, delayTime = spectrum3(5), decayTime = spectrum3(10))
      .withNrOfChannels(2)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)
    delay.getOutputBus.staticBus(0)
    val graph = delay.buildGraph(startTime, duration, delay.graph(Seq()))
    player.sendNew(absoluteTimeToMillis(startTime), graph)

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.1f, 0.2f), freq = (spectrum3(0) / 11, spectrum3(1) / 11, spectrum3(1) / 11, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(0), spectrum(0), spectrum(0), spectrum(0)),
        higherFreq = (spectrum(1), spectrum(1), spectrum(1), spectrum(1)))
      .ring(ringModFreq = (spectrum(2), spectrum(2), spectrum(2), spectrum(2)))     
      .pan(panValue = (-0.8f, 0, 0, 0.8f), output = Some(delayAudioBus))
      .play 

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.6f, 0.5f), freq = (spectrum3(0) / 11, (spectrum3(1) / 11) * 1.004f, (spectrum3(1) / 11) * 0.997f, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(8), spectrum(9), spectrum(9), spectrum(8)), 
        higherFreq = (spectrum(17), spectrum(14), spectrum(14), spectrum(17)))   
      .ring(ringModFreq = (spectrum(11), spectrum(11), spectrum(11), spectrum(11)))    
      .pan(panValue = (0f, 0.5f, -0.5f, 0f), output = Some(delayAudioBus))
      .play       

    Note(startTime = startTime, duration = duration, lengths = pulseLengths)
      .pulse(ampValue = (0.8f, 0.9f), freq = (spectrum3(0) / 11, spectrum3(1) / 11 * 0.997f, (spectrum3(1) / 11) * 1.004f, spectrum3(2) / 11), startValue = 0.1f)
      .bandPass(
        lowerFreq = (spectrum(38), spectrum(39), spectrum(39), spectrum(38)), 
        higherFreq = (spectrum(49), spectrum(48), spectrum(48), spectrum(49)))   
      .ring(ringModFreq = (spectrum(42), spectrum(42), spectrum(42), spectrum(42)))    
      .pan(panValue = (0.6f, -0.3f, 0.3f, -0.6f), output = Some(delayAudioBus))
      .play 

    // Middle theme
    val longDurationDivision = duration / 4
    val longStartTimes = absolute(startTime + longDurationDivision, Seq(longDurationDivision, longDurationDivision, longDurationDivision))

    val middleThemeDuration = spectrum3(20)
    val middleThemePulse = middleThemeDuration / 8

    println(s"long start times $longStartTimes")
    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse *1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq =   (spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact, spectrum(3) * fact),
          carrierFreq = (spectrum(3), spectrum(3), spectrum(3), spectrum(3)),
          modAmount = (100, 300, 3000, 300))
       .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
       .play

    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact, spectrum(4) * fact),
          carrierFreq = (spectrum(4), spectrum(4), spectrum(4), spectrum(4)),
          modAmount = (100, 2000, 500, 100))   
       .pan(panValue = (-0.8f, 0, 0, 0.8f))
       .play  
       
    Note(startTime = longStartTimes.head, duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact, spectrum(5) * fact),
          carrierFreq = (spectrum(5), spectrum(5), spectrum(5), spectrum(5)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play  
      
  
    // Higher theme
    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(6) * fact, spectrum(6) * fact, spectrum(6) * fact, spectrum(6) * fact),
          carrierFreq = (spectrum(6), spectrum(6), spectrum(6), spectrum(6)),
          modAmount = (100, 300, 3000, 300))
      .pan(panValue = (-0.5f, -0.5f, -0.5f, -0.5f))
      .play    


    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),  
          modFreq = (spectrum(9) * fact, spectrum(9) * fact, spectrum(9) * fact, spectrum(9) * fact),
          carrierFreq = (spectrum(9), spectrum(9), spectrum(9), spectrum(9)),
          modAmount = (100, 2000, 500, 100))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play

    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(12) * fact, spectrum(12) * fact, spectrum(12) * fact, spectrum(12) * fact),
          carrierFreq = (spectrum(12), spectrum(12), spectrum(12), spectrum(12)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0.5f, 0.5f, 0.5f, 0.5f))
      .play  

    Note(startTime = longStartTimes(1), duration = middleThemeDuration, lengths = (middleThemePulse * 2, middleThemePulse * 5, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f), 
          modFreq = (spectrum(15) * fact, spectrum(15) * fact, spectrum(15) * fact, spectrum(15) * fact),
          carrierFreq = (spectrum(15), spectrum(15), spectrum(15), spectrum(15)),
          modAmount = (100, 500, 3000, 300))
      .pan(panValue = (0f, 0f, 0f, 0f))
      .play 

    // Lower theme
    Note(startTime = longStartTimes(2), duration = middleThemeDuration, lengths = (middleThemePulse * 1, middleThemePulse * 5, middleThemePulse * 2))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(0) * fact, spectrum(0) * fact, spectrum(0) * fact, spectrum(0) * fact),
          carrierFreq = (spectrum(0), spectrum(0), spectrum(0), spectrum(0)),
          modAmount = (100, 300, 2000, 200))
      .pan(panValue = (0.5f, 0.5f, -0.5f, -0.5f))
      .play    

    Note(startTime = longStartTimes(2), duration = middleThemeDuration, lengths = (middleThemePulse * 3, middleThemePulse * 4, middleThemePulse * 1))
      .fm(ampValue = (0.5f, 0.5f),
          modFreq = (spectrum(1) * fact, spectrum(1) * fact, spectrum(1) * fact, spectrum(1) * fact),
          carrierFreq = (spectrum(1), spectrum(1), spectrum(1), spectrum(1)),
          modAmount = (100, 2000, 500, 200))
      .pan(panValue = (-0.8f, 0, 0, 0.8f))
      .play 
  }

  def main(args: Array[String]): Unit = {
    implicit val player: MusicPlayer = MusicPlayer()
    player.startPlay()
    setupNodes(player)

    val startTimes = absolute(0f, Seq(
      65.4064f + 4.1410513f, 
      77.78175f + 3.558817f, 
      116.540955f + 5.0539646f,
      69.29566f  + 4.8818545f,
      61.735424f + 3.810793f,
      77.78175f + 5.5251265f,
      73.416214f + 5.6364136f))

    println(s"Start times $startTimes")
  

    exposition1(startTimes.head)
    exposition2(startTimes(1))
    exposition3(startTimes(2))
    exposition4(startTimes(3)) 
    exposition5(startTimes(4)) 
    exposition6(startTimes(5))
    exposition7(startTimes(6))
    
    Console.println("Print q to quit")
    
    val cmd = StdIn.readLine()
    Console.println(s"You typed $cmd, goodBye")
    player.stopPlay()
  }
}
