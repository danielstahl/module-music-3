
// Eli explains his setup
// "https://www.youtube.com/watch?v=P85X1Ut3Hfc"

(
s = Server.local;
//s.options.outDevice = "Built-in Output";
//s.options.outDevice = "Scarlett 2i2 USB";
//s.options.outDevice = "MOMENTUM M2 AEBT";
//s.options.outDevice = "Soundflower (2ch)";
s.options.outDevice = "Soundflower Aggregate";
s.options.numOutputBusChannels = 66;
s.options.blockSize = 128;
s.options.sampleRate = 96000;
s.options.numBuffers = 2056;
s.options.memSize = 2.pow(20);
s.dumpOSC(1);
)

(
SynthDef(\staticControl, {
	arg dur = 1, value = 1, out = 0;
	var lineEnv;
	lineEnv = Line.kr(start: value, end: value, dur: dur, doneAction:2);
	Out.kr(out, lineEnv);
}).add;

SynthDef(\lineControl, {
	arg dur = 1, startValue = 1, endValue = 1, out = 0;
	var lineEnv;
	lineEnv = Line.kr(start: startValue, end: endValue, dur: dur, doneAction:2);
	Out.kr(out, lineEnv);
}).add;

SynthDef(\xlineControl, {
	arg dur = 1, startValue = 1, endValue = 1, out = 0;
	var xlineEnv;
	xlineEnv = XLine.kr(start: startValue, end: endValue, dur: dur, doneAction:2);
	Out.kr(out, xlineEnv);
}).add;

SynthDef(\percControl, {
	arg dur = 1, attackTime = 0.01, curve = -4, startValue = 1, peakValue = 1, out = 0;
	var percEnv;
	percEnv = EnvGen.kr(
		Env.perc(attackTime, dur - attackTime, 1, curve), doneAction:2).linlin(0, 1, startValue, peakValue);
	Out.kr(out, percEnv);
}).add;

SynthDef(\sineControl, {
	arg dur = 1, freqBus = 0, minValue = 1, maxValue = 1, out = 0;
	var freq, sig;
	Line.kr(dur:dur, doneAction:2);
	freq = In.kr(freqBus, 1);
	sig = SinOsc.kr(freq).linlin(-1, 1, minValue, maxValue);
	Out.kr(out, sig);
}).add;

SynthDef(\controlMix, {
	arg dur = 1, in1 = 0, in2 = 0, out = 0;
	var sig1, sig2, result;
	Line.kr(dur:dur, doneAction:2);
	sig1 = In.kr(in1, 1);
	sig2 = In.kr(in2, 1);
	result = Mix.new([sig1, sig2]);
	Out.kr(out, result);
}).add;

SynthDef(\threeBlockControl, {
	arg dur = 1, startValue1, len1, startValue2, len2, startValue3, len3, endValue3, curve = 'lin', out = 0;
	var threeBlockEnv;
	threeBlockEnv = EnvGen.kr(
		Env.new(
			levels: [startValue1, startValue2, startValue3, endValue3],
			times: [len1, len2, len3],
			curve: curve),
		doneAction:2);
	Out.kr(out, threeBlockEnv);
}).add;

SynthDef(\whiteNoiseOsc, {
	arg dur = 1, ampBus = 0, out = 0;
	var noise, amp;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	noise = WhiteNoise.ar(amp);
	Out.ar(out, noise);
}).add;

SynthDef(\sawOsc, {
	arg dur = 1, freqBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(freqBus, 1);
	sig = LFSaw.ar(freq, mul:amp);
	Out.ar(out, sig);
}).add;

SynthDef(\pulseOsc, {
	arg dur = 1, freqBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(freqBus, 1);
	sig = Pulse.ar(freq, mul:amp);
	Out.ar(out, sig);
}).add;

SynthDef(\sineOsc, {
	arg dur = 1, freqBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(freqBus, 1);
	sig = SinOsc.ar(freq, mul:amp);
	Out.ar(out, sig);
}).add;

SynthDef(\triangleOsc, {
	arg dur = 1, freqBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(freqBus, 1);
	sig = LFTri.ar(freq, mul:amp);
	Out.ar(out, sig);
}).add;

SynthDef(\dustOsc, {
	arg dur = 1, freqBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(freqBus, 1);
	sig = Dust.ar(freq, mul:amp);
	Out.ar(out, sig);
}).add;

SynthDef(\xfade, {
	arg dur = 1, in1 = 0, in2 = 0, xfadeBus = 0, out = 0;
	var sig1, sig2, pan, result;
	Line.kr(dur:dur, doneAction:2);
	sig1 = In.ar(in1, 1);
	sig2 = In.ar(in2, 1);
	pan = In.kr(xfadeBus, 1);
	result = XFade2.ar(sig1, sig2, pan);
	Out.ar(out, result);
}).add;

SynthDef(\mix, {
	arg dur = 1, in1 = 0, in2 = 0, out = 0;
	var sig1, sig2, result;
	Line.kr(dur:dur, doneAction:2);
	sig1 = In.ar(in1, 1);
	sig2 = In.ar(in2, 1);
	result = Mix.new([sig1, sig2]);
	Out.ar(out, result);
}).add;

SynthDef(\moogFilter, {
	arg dur = 1, in = 0, freqBus = 0, gainBus = 0, out = 0;
	var input, sig, freq, gain;
	Line.kr(dur:dur, doneAction:2);
	input = In.ar(in, 1);
	freq = In.kr(freqBus, 1);
	gain = In.kr(gainBus, 1);
	sig = MoogFF.ar(input, freq, gain);
	Out.ar(out, sig);
}).add;

SynthDef(\resonantFilter, {
	arg dur = 1, in = 0, freqBus = 0, decayBus = 0, out = 0;
	var input, sig, freq, decay;
	Line.kr(dur:dur, doneAction:2);
	input = In.ar(in, 1);
	freq = In.kr(freqBus, 1);
	decay = In.kr(decayBus, 1);
	sig = Ringz.ar(input, freq, decay);
	Out.ar(out, sig);
}).add;

SynthDef(\highPassFilter, {
	arg dur = 1, in = 0, freqBus = 0, out = 0;
	var input, sig, freq;
	Line.kr(dur:dur, doneAction:2);
	input = In.ar(in, 1);
	freq = In.kr(freqBus, 1);
	sig = HPF.ar(input, freq);
	Out.ar(out, sig);
}).add;

SynthDef(\lowPassFilter, {
	arg dur = 1, in = 0, freqBus = 0, out = 0;
	var input, sig, freq;
	Line.kr(dur:dur, doneAction:2);
	input = In.ar(in, 1);
	freq = In.kr(freqBus, 1);
	sig = LPF.ar(input, freq);
	Out.ar(out, sig);
}).add;

SynthDef(\bandPassFilter, {
	arg dur = 1, in = 0, freqBus = 0, rqBus = 0, out = 0;
	var input, sig, freq, rq;
	Line.kr(dur:dur, doneAction:2);
	input = In.ar(in, 1);
	freq = In.kr(freqBus, 1);
	rq = In.kr(rqBus, 1);
	sig = BPF.ar(input, freq, rq);
	Out.ar(out, sig);
}).add;

SynthDef(\ringModulate, {
	arg dur = 1, carrierBus = 0, modulatorFreqBus = 0, out = 0;
	var carrier, modulatorFreq, sig;
	Line.kr(dur:dur, doneAction:2);
	carrier = In.ar(carrierBus, 1);
	modulatorFreq = In.kr(modulatorFreqBus, 1);
	sig = carrier * SinOsc.ar(modulatorFreq, 0, 0.5);
	Out.ar(out, sig);
}).add;

SynthDef(\amModulate, {
	arg dur = 1, carrierBus = 0, modulatorFreqBus = 0, out = 0;
	var carrier, modulatorFreq, sig;
	Line.kr(dur:dur, doneAction:2);
	carrier = In.ar(carrierBus, 1);
	modulatorFreq = In.ar(modulatorFreqBus, 1);
	sig = carrier * SinOsc.ar(modulatorFreq, 0, 0.25, 0.25);
	Out.ar(out, sig);
}).add;

SynthDef(\pmModulate, {
	arg dur = 1, carrierFreqBus = 0, modulatorFreqBus = 0, modulatorIndexBus = 0, ampBus = 0, out = 0;
	var carrierFreq, modulatorFreq, modulatorIndex, sig, conversion = 2pi/(s.sampleRate), amp;
	Line.kr(dur:dur, doneAction:2);
	carrierFreq = In.kr(carrierFreqBus, 1);
	modulatorFreq = In.kr(modulatorFreqBus, 1);
	modulatorIndex = In.kr(modulatorIndexBus, 1);
	amp = In.kr(ampBus, 1);

	sig = SinOsc.ar(
			carrierFreq,
			modulatorFreq * modulatorIndex * conversion * SinOsc.ar(modulatorFreq), 0.25);
	Out.ar(out, sig * amp);
}).add;

SynthDef(\fmSineModulate, {
	arg dur = 1, carrierFreqBus = 0, modulatorBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq, mod;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(carrierFreqBus, 1);
	mod = In.ar(modulatorBus, 1);

	sig = SinOsc.ar(freq + mod, mul:amp);
	Out.ar(out, sig);
}).add;

SynthDef(\fmPulseModulate, {
	arg dur = 1, carrierFreqBus = 0, modulatorBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq, mod;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(carrierFreqBus, 1);
	mod = In.ar(modulatorBus, 1);

	sig = Pulse.ar(freq + mod, mul:amp);
	Out.ar(out, sig);
}).add;

SynthDef(\fmSawModulate, {
	arg dur = 1, carrierFreqBus = 0, modulatorBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq, mod;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(carrierFreqBus, 1);
	mod = In.ar(modulatorBus, 1);

	sig = LFSaw.ar(freq + mod, mul:amp);
	Out.ar(out, sig);
}).add;

SynthDef(\fmTriangleModulate, {
	arg dur = 1, carrierFreqBus = 0, modulatorBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq, mod;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(carrierFreqBus, 1);
	mod = In.ar(modulatorBus, 1);

	sig = LFTri.ar(freq + mod, mul:amp);
	Out.ar(out, sig);
}).add;

SynthDef(\pmSineOsc, {
	arg dur = 1, freqBus = 0, modBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq, mod, conversion= 2pi/(s.sampleRate);
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(freqBus, 1);
	mod = In.ar(modBus, 1);

	sig = SinOsc.ar(freq, mod * conversion, mul:amp);
	Out.ar(out, sig);
}).add;

SynthDef(\pan, {arg dur = 1, panBus = 0, in = 0, out = 0;
	var pan, input;
	Line.kr(dur:dur, doneAction:2);
	pan = In.kr(panBus, 1);

	input = In.ar(in, 1);
	Out.ar(out, LinPan2.ar(input, pan));
}).add;

SynthDef(\monoDelay, {
	arg dur = 1, in = 0, out = 0, delaytime = 0.2, decaytime = 1, ampBus = 0;
	var input, delaySig, amp;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	input = In.ar(in, 1);
	delaySig = CombL.ar(input, delaytime, delaytime, decaytime, mul:amp);

	Out.ar(out, delaySig + input);
}).add;

SynthDef(\stereoDelay, {
	arg dur = 1, in = 0, out = 0, delaytime = 0.2, decaytime = 1, ampBus = 0;
	var input, delaySig, amp;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	input = In.ar(in, 2);
	delaySig = CombL.ar(input, delaytime, delaytime, decaytime, mul:amp);

	Out.ar(out, delaySig + input);
}).add;

SynthDef(\reverb, {
	arg in = 0, predelay = 0.048, wet = 0.2, combdelay = 0.1, combdecay = 5, allpassdelay = 0.05, allpassdecay = 1;
	var sig, input;

	input = input=In.ar(in,2);
	// Predelay
	sig = DelayN.ar(input, predelay);

	// 7 Comb filters
	sig = Mix.fill(7, {CombL.ar(sig, combdelay, Rand(0.01, combdelay), combdecay)});
	// 4 Allpass
	4.do({ sig = AllpassN.ar(sig, allpassdelay + 0.001, [Rand(0.01, allpassdelay), Rand(0.01, allpassdelay)], allpassdecay) });

	Out.ar(0, input + (sig * wet));
}).add;

"Added Instruments".postln;
)



(
~simplePatch = {
	arg pan = 0, dur = 1, freq = 440;
	var amp = Bus.control(s, 1), panBus = Bus.control(s, 1), freqBus = Bus.control(s, 1), sig = Bus.audio(s, 1);
	Synth(\percControl, [\out, amp, \dur, dur, \attackTime, 0.01, \startValue, 0.001, \peakValue, 1, \curve, \lin]);
	Synth(\staticControl, [\out, panBus, \dur, dur, \value, pan]);
	Synth(\staticControl, [\out, freqBus, \dur, dur, \value, freq]);
	Synth(\sineOsc, [\out, sig, \dur, dur, \ampBus, amp, \freqBus, freqBus], addAction:\addToTail);
	Synth(\pan, [\in, sig, \panBus, panBus, \out, 0, \dur, dur], addAction:\addToTail);
};
)
[62, 65, 68].do({
	arg item, i;
	~simplePatch.value(0, 2, item.midicps);
});



(
var amp = Bus.control(s, 1), panBus = Bus.control(s, 1), freqBus = Bus.control(s, 1), sig = Bus.audio(s, 1), dur = 5;
Synth(\percControl, [\out, amp, \dur, dur, \attackTime, 1, \startValue, 0.001, \peakValue, 1, \curve, \lin]);
Synth(\lineControl, [\out, panBus, \dur, dur, \startValue, -1, \endValue, 1]);
Synth(\staticControl, [\out, freqBus, \dur, dur, \value, 880]);
Synth(\triangleOsc, [\out, sig, \dur, dur, \ampBus, amp, \freqBus, freqBus], addAction:\addToTail);
Synth(\pan, [\in, sig, \panBus, panBus, \out, 0, \dur, dur], addAction:\addToTail);
)

(
var amp = Bus.control(s, 1), panBus = Bus.control(s, 1), freqBus = Bus.control(s, 1),
sig1 = Bus.audio(s, 1), sig2 = Bus.audio(s, 1), sig3 = Bus.audio(s, 1), xfade = Bus.control(s, 1), dur = 5;
Synth(\percControl, [\out, amp, \dur, dur, \attackTime, 2, \startValue, 0.001, \peakValue, 1, \curve, \lin]);
Synth(\lineControl, [\out, panBus, \dur, dur, \startValue, -1, \endValue, 1]);
Synth(\staticControl, [\out, freqBus, \dur, dur, \value, 880]);
Synth(\triangleOsc, [\out, sig1, \dur, dur, \ampBus, amp, \freqBus, freqBus], addAction:\addToTail);
Synth(\pulseOsc, [\out, sig2, \dur, dur, \ampBus, amp, \freqBus, freqBus], addAction:\addToTail);
Synth(\lineControl, [\out, xfade, \dur, dur, \startValue, -1, \endValue, 1]);
Synth(\xfade, [\in1, sig1, \in2, sig2, \out, sig3, \xfadeBus, xfade, \dur, dur], addAction:\addToTail);
Synth(\pan, [\in, sig3, \panBus, panBus, \out, 0, \dur, dur], addAction:\addToTail);
)

(
var amp = Bus.control(s, 1), panBus = Bus.control(s, 1), freqBus = Bus.control(s, 1), sig = Bus.audio(s, 1), dur = 8,
filterFreqBus = Bus.control(s, 1), filterGainBus = Bus.control(s, 1), filterOut = Bus.audio(s, 1);
Synth(\percControl, [\out, amp, \dur, dur, \attackTime, 3, \startValue, 0.001, \peakValue, 1, \curve, \lin]);
Synth(\lineControl, [\out, panBus, \dur, dur, \startValue, -1, \endValue, 1]);
Synth(\staticControl, [\out, freqBus, \dur, dur, \value, 40]);
Synth(\lineControl, [\out, filterFreqBus, \dur, dur, \startValue, 40, \endValue, 5000]);
Synth(\staticControl, [\out, filterGainBus, \dur, dur, \value, 3.5]);
Synth(\pulseOsc, [\out, sig, \dur, dur, \ampBus, amp, \freqBus, freqBus], addAction:\addToTail);
Synth(\moogFilter, [\out, filterOut, \in, sig, \dur, dur, \freqBus, filterFreqBus, \gainBus, filterGainBus], addAction:\addToTail);
Synth(\pan, [\in, filterOut, \panBus, panBus, \out, 0, \dur, dur], addAction:\addToTail);
)

(
var amp = Bus.control(s, 1), panBus = Bus.control(s, 1), sig = Bus.audio(s, 1), dur = 8,
filterFreqBus = Bus.control(s, 1), filterDecayBus = Bus.control(s, 1), filterOut = Bus.audio(s, 1);
Synth(\percControl, [\out, amp, \dur, dur, \attackTime, 3, \startValue, 0.001, \peakValue, 0.1, \curve, \lin]);
Synth(\lineControl, [\out, panBus, \dur, dur, \startValue, -1, \endValue, 1]);
Synth(\lineControl, [\out, filterFreqBus, \dur, dur, \startValue, 4000, \endValue, 500]);
Synth(\staticControl, [\out, filterDecayBus, \dur, dur, \value, 0.1]);
Synth(\whiteNoiseOsc, [\out, sig, \dur, dur, \ampBus, amp], addAction:\addToTail);
Synth(\resonantFilter, [\out, filterOut, \in, sig, \dur, dur, \freqBus, filterFreqBus, \decayBus, filterDecayBus], addAction:\addToTail);
Synth(\pan, [\in, filterOut, \panBus, panBus, \out, 0, \dur, dur], addAction:\addToTail);
)

(
var amp = Bus.control(s, 1), panBus = Bus.control(s, 1), freqBus = Bus.control(s, 1), sig = Bus.audio(s, 1), dur = 13,
modFreqBus = Bus.audio(s, 1), modOut = Bus.audio(s, 1);
Synth(\percControl, [\out, amp, \dur, dur, \attackTime, 3, \startValue, 0.001, \peakValue, 1, \curve, \lin]);
Synth(\lineControl, [\out, panBus, \dur, dur, \startValue, -1, \endValue, 1]);
Synth(\staticControl, [\out, freqBus, \dur, dur, \value, 50.midicps]);
Synth(\staticControl, [\out, modFreqBus, \dur, dur, \value, 62.midicps]);
Synth(\pulseOsc, [\out, sig, \dur, dur, \ampBus, amp, \freqBus, freqBus], addAction:\addToTail);
Synth(\ringModulate, [\out, modOut, \carrierBus, sig, \dur, dur, \modulatorFreqBus, modFreqBus], addAction:\addToTail);
Synth(\pan, [\in, modOut, \panBus, panBus, \out, 0, \dur, dur], addAction:\addToTail);
)

(
var amp = Bus.control(s, 1), panBus = Bus.control(s, 1), freqBus = Bus.control(s, 1), sig = Bus.audio(s, 1), dur = 13,
modFreqBus = Bus.audio(s, 1), modOut = Bus.audio(s, 1);
Synth(\percControl, [\out, amp, \dur, dur, \attackTime, 3, \startValue, 0.001, \peakValue, 1, \curve, \lin]);
Synth(\lineControl, [\out, panBus, \dur, dur, \startValue, -1, \endValue, 1]);
Synth(\staticControl, [\out, freqBus, \dur, dur, \value, 55.midicps]);
Synth(\staticControl, [\out, modFreqBus, \dur, dur, \value, 62.midicps]);
Synth(\pulseOsc, [\out, sig, \dur, dur, \ampBus, amp, \freqBus, freqBus], addAction:\addToTail);
Synth(\amModulate, [\out, modOut, \carrierBus, sig, \dur, dur, \modulatorFreqBus, modFreqBus], addAction:\addToTail);
Synth(\pan, [\in, modOut, \panBus, panBus, \out, 0, \dur, dur], addAction:\addToTail);
)

(
var amp = Bus.control(s, 1), panBus = Bus.control(s, 1), carrierFreqBus = Bus.control(s, 1), modFreqBus = Bus.control(s, 1),
modIndexBus = Bus.control(s, 1), dur = 13, modOut = Bus.audio(s, 1);
Synth(\percControl, [\out, amp, \dur, dur, \attackTime, 3, \startValue, 0.001, \peakValue, 1, \curve, \lin]);
Synth(\lineControl, [\out, panBus, \dur, dur, \startValue, -1, \endValue, 1]);
Synth(\staticControl, [\out, carrierFreqBus, \dur, dur, \value, 55.midicps]);
Synth(\staticControl, [\out, modFreqBus, \dur, dur, \value, 62.midicps]);
Synth(\staticControl, [\out, modIndexBus, \dur, dur, \value, 4]);
Synth(\fmModulate, [\out, modOut, \carrierFreqBus, carrierFreqBus, \modulatorFreqBus, modFreqBus, \modulatorIndexBus, modIndexBus, \dur, dur, \ampBus, amp], addAction:\addToTail);
Synth(\pan, [\in, modOut, \panBus, panBus, \out, 0, \dur, dur], addAction:\addToTail);
)

(
var amp = Bus.control(s, 1), panBus = Bus.control(s, 1), carrierFreqBus = Bus.control(s, 1), modFreqBus = Bus.control(s, 1),
modIndexBus = Bus.control(s, 1), dur = 13, modOut = Bus.audio(s, 1);
Synth(\percControl, [\out, amp, \dur, dur, \attackTime, 3, \startValue, 0.001, \peakValue, 1, \curve, \lin]);
Synth(\lineControl, [\out, panBus, \dur, dur, \startValue, -1, \endValue, 1]);
Synth(\staticControl, [\out, carrierFreqBus, \dur, dur, \value, 55.midicps]);
Synth(\staticControl, [\out, modFreqBus, \dur, dur, \value, 62.midicps]);
Synth(\staticControl, [\out, modIndexBus, \dur, dur, \value, 34]);
Synth(\pmModulate, [\out, modOut, \carrierFreqBus, carrierFreqBus, \modulatorFreqBus, modFreqBus, \modulatorIndexBus, modIndexBus, \dur, dur, \ampBus, amp], addAction:\addToTail);
Synth(\pan, [\in, modOut, \panBus, panBus, \out, 0, \dur, dur], addAction:\addToTail);
)

(
var amp = Bus.control(s, 1), panBus = Bus.control(s, 1), carrierFreqBus = Bus.control(s, 1), modFreqBus = Bus.control(s, 1),
modIndexBus = Bus.control(s, 1), dur = 13, modOut = Bus.audio(s, 1), partialBus = Bus.control(s, 1),
indexBus = Bus.control(s, 1), sigOut = Bus.audio(s, 1);
Synth(\percControl, [\out, amp, \dur, dur, \attackTime, 3, \startValue, 0.001, \peakValue, 1, \curve, \lin]);
Synth(\lineControl, [\out, panBus, \dur, dur, \startValue, -1, \endValue, 1]);
Synth(\staticControl, [\out, carrierFreqBus, \dur, dur, \value, 55.midicps]);
Synth(\staticControl, [\out, modFreqBus, \dur, dur, \value, 62.midicps]);
Synth(\staticControl, [\out, modIndexBus, \dur, dur, \value, 34]);
Synth(\lineControl, [\out, indexBus, \dur, dur, \startValue, 3, \endValue, 14]);
Synth(\lineControl, [\out, partialBus, \dur, dur, \startValue, 1, \endValue, 1]);
Synth(\fmModulator, [\dur, dur, \out, modOut, \freqBus, modFreqBus,  \partialBus, partialBus, \ampBus, amp, \indexBus, indexBus], addAction:\addToTail);
Synth(\fmCarrier, [\dur, dur, \out, sigOut, \freqBus, carrierFreqBus, \modBus, modOut, \partialBus, partialBus, \ampBus, amp], addAction:\addToTail);

Synth(\pan, [\in, sigOut, \panBus, panBus, \out, 0, \dur, dur], addAction:\addToTail);
)


(
var amp = Bus.control(s, 1), panBus = Bus.control(s, 1), carrierFreqBus = Bus.control(s, 1), modFreqBus = Bus.control(s, 1),
modFreqBus2 = Bus.control(s, 1), dur = 13, modOut = Bus.audio(s, 1), partialBus = Bus.control(s, 1),
indexBus = Bus.control(s, 1), indexBus2 = Bus.control(s, 1), sigOut = Bus.audio(s, 1);
Synth(\percControl, [\out, amp, \dur, dur, \attackTime, 3, \startValue, 0.001, \peakValue, 1, \curve, \lin]);
Synth(\lineControl, [\out, panBus, \dur, dur, \startValue, -1, \endValue, 1]);
Synth(\staticControl, [\out, carrierFreqBus, \dur, dur, \value, 55.midicps]);
Synth(\staticControl, [\out, modFreqBus, \dur, dur, \value, 62.midicps]);
Synth(\staticControl, [\out, modFreqBus2, \dur, dur, \value, 67.midicps]);

Synth(\lineControl, [\out, indexBus, \dur, dur, \startValue, 3, \endValue, 14]);
Synth(\lineControl, [\out, indexBus2, \dur, dur, \startValue, 7, \endValue, 2]);
Synth(\lineControl, [\out, partialBus, \dur, dur, \startValue, 1, \endValue, 1]);
Synth(\fmModulator, [\dur, dur, \out, modOut, \freqBus, modFreqBus,  \partialBus, partialBus, \ampBus, amp, \indexBus, indexBus], addAction:\addToTail);
Synth(\fmModulator, [\dur, dur, \out, modOut, \freqBus, modFreqBus2,  \partialBus, partialBus, \ampBus, amp, \indexBus, indexBus2], addAction:\addToTail);
Synth(\fmCarrier, [\dur, dur, \out, sigOut, \freqBus, carrierFreqBus, \modBus, modOut, \partialBus, partialBus, \ampBus, amp], addAction:\addToTail);

Synth(\pan, [\in, sigOut, \panBus, panBus, \out, 0, \dur, dur], addAction:\addToTail);
)

s.reboot;

// Separate modulator and carrier
// See "http://danielnouri.org/docs/SuperColliderHelp/Tutorials/Mark_Polishook_tutorial/Synthesis/14_Frequency_modulation.html"

// FM Synthesis algorithms
// "https://www.youtube.com/watch?v=Y1xPT5D7Oc0"

// Filter modulation with amModulate?
// https://www.soundonsound.com/techniques/amplitude-modulation

//s.prepareForRecord('/Users/danielstahl/Documents/Projects/modular-i_mix/mvnt-I_sc.aiff');
s.prepareForRecord();
s.record;
s.stopRecording;



