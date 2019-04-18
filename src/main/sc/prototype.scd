
// Prototypes

(
SynthDef(\sawsynth, {
	arg dur = 1, freq = 440, amp = 0.5, attack = 0.02, pan = 0;
	var env, sig;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp), doneAction:2);
	sig = LFSaw.ar(freq, mul:env);
	Out.ar(0, PanX.ar(2, sig, pan.linlin(-1, 1, 0, 1)));
}).add;

// General version of a Square wave
SynthDef(\pulsesynth, {
	arg dur = 1, freq = 440, amp = 0.5, attack = 0.02, pan = 0;
	var env, sig;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp), doneAction:2);
	sig = Pulse.ar(freq) * env;
	Out.ar(0, PanX.ar(2, sig, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\sinesynth, {
	arg dur = 1, freq = 440, amp = 0.5, attack = 0.02, pan = 0;
	var env, sig;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp), doneAction:2);
	sig = SinOsc.ar(freq) * env;
	Out.ar(0, PanX.ar(2, sig, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\trianglesynth, {
	arg dur = 1, freq = 440, amp = 0.5, attack = 0.02, pan = 0;
	var env, sig;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp), doneAction:2);
	sig = LFTri.ar(freq) * env;
	Out.ar(0, PanX.ar(2, sig, pan.linlin(-1, 1, 0, 1)));
}).add;
)

Synth(\pulsesynth, [\freq, 220, \attack, 0.05, \dur, 1, \amp, 0.6, \pan, 0.7]);
Synth(\pulsesynth, [\freq, 110, \attack, 0.05, \dur, 1, \amp, 0.6, \pan, 0.5]);
Synth(\pulsesynth, [\freq, 55, \attack, 0.05, \dur, 1, \amp, 0.6, \pan, 0.3]);

Synth(\sawsynth, [\freq, 110, \attack, 0.05, \dur, 2, \amp, 0.6, \pan, 0.3]);
Synth(\sinesynth, [\freq, 55.1, \attack, 0.05, \dur, 2, \amp, 0.6, \pan, 0.7]);

(
var saw, pulse, longtri, longpulse;

pulse = Pbind(\instrument, \sawsynth,
	\midinote, Pseq([57, 57, 60, 64], inf),
	\attack, 0.05,
	\pan, Pxrand([1, 0.25, 0.5, 0], inf),
	\dur, Prand([Pseq([0.25, 0.25]), Pseq([0.125, 0.125, 0.125, 0.125])], inf),
	\amp, Pxrand([0.2, 0.3, 0.1], inf));

saw = Pbind(\instrument, \pulsesynth,
	\midinote, Pseq([67, 60, 57, 64], inf),
	\attack, 0.05,
	\pan, Pxrand([-1, -0.25, -0.5, 0], inf),
	\dur, Prand([Pseq([0.25, 0.25]), Pseq([0.125, 0.125, 0.125, 0.125])], inf),
	\amp, Pxrand([0.2, 0.3, 0.1], inf));


longtri = Pbind(\instrument, \trianglesynth,
	\midinote, Pxrand([45, 52, 55, 48], inf),
	\attack, 0.5,
	\pan, Pxrand([-0.75, -0.5, 0, 0.5, 0.75], inf),
	\dur, Prand([Pseq([2, 3]), Pseq([2, 3, 2])], inf),
	\amp, Pxrand([0.2, 0.3, 0.1], inf));

longpulse = Pbind(\instrument, \pulsesynth,
	\midinote, Pxrand([33, 40, 43, 36], inf),
	\attack, 1,
	\pan, Pxrand([-1, -0.25, 0.25, 1], inf),
	\dur, Prand([5, 8], inf),
	\amp, Pxrand([0.2, 0.1, 0.05], inf)).play;

Ppar([saw, pulse, longtri, longpulse]).play;
)

// Moog bass synth "https://www.youtube.com/watch?v=PCK7TlwSJhs"

/*
osc 1 32 square wave
osc 2 16 saw tooth wave
osc 3 up octave saw tooth

Filter with envelope (lowpass filter). Cut off frequency.
*/

(
SynthDef(\pulsefiltersynth, {
	arg dur = 1, freq = 440, amp = 0.5, attack = 0.02, pan = 0,
	filterattack = 0.02, startcutoff = 440, peakcutoff = 440, filtergain = 2, ampcurve = -4, filtercurve = -4;
	var env, sig, filterenv;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp, curve:ampcurve), doneAction:2);
	filterenv = EnvGen.kr(Env.perc(filterattack, dur - filterattack, 1, curve:filtercurve), doneAction:2).linlin(0, 1, startcutoff, peakcutoff);
	sig = Pulse.ar(freq) * env;
	sig = MoogFF.ar(sig, filterenv, filtergain);
	Out.ar(0, PanX.ar(2, sig, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\sawfiltersynth, {
	arg dur = 1, freq = 440, amp = 0.5, attack = 0.02, pan = 0,
	filterattack = 0.02, startcutoff = 440, peakcutoff = 440, filtergain = 2, ampcurve = -4, filtercurve = -4;
	var env, sig, filterenv;
		env = EnvGen.kr(Env.perc(attack, dur - attack, amp, curve:ampcurve), doneAction:2);
	filterenv = EnvGen.kr(Env.perc(filterattack, dur - filterattack, 1, curve:filtercurve), doneAction:2).linlin(0, 1, startcutoff, peakcutoff);
	sig = LFSaw.ar(freq) * env;
	sig = MoogFF.ar(sig, filterenv, filtergain);
	Out.ar(0, PanX.ar(2, sig, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\whitenoisemoogfilter, {
	arg dur = 1, amp = 0.5, attack = 0.02, pan = 0,
	filterattack = 0.02, startcutoff = 440, peakcutoff = 440, filtergain = 2, ampcurve = -4, filtercurve = -4;
	var env, sig, filterenv;
		env = EnvGen.kr(Env.perc(attack, dur - attack, amp, curve:ampcurve), doneAction:2);
	filterenv = EnvGen.kr(Env.perc(filterattack, dur - filterattack, 1, curve:filtercurve), doneAction:2).linlin(0, 1, startcutoff, peakcutoff);
	sig = WhiteNoise.ar() * env;
	sig = MoogFF.ar(sig, filterenv, filtergain);
	Out.ar(0, PanX.ar(2, sig, pan.linlin(-1, 1, 0, 1)));
}).add;


SynthDef(\noiseringzfilter, {
	arg dur = 1, amp = 0.5, attack = 0.02, pan = 0,
	freq = 440, decayattack = 0.02, startdecay = 2, peakdecay = 2, ampcurve = -4, decaycurve = - 4;
	var ampenv, sig, decayenv;

	ampenv = EnvGen.kr(Env.perc(attack, dur - attack, amp, curve:ampcurve), doneAction:2);
	decayenv = EnvGen.kr(Env.perc(decayattack, dur - decayattack, 1, curve:decaycurve), doneAction:2).linlin(0, 1, startdecay, peakdecay);
	sig = WhiteNoise.ar() * ampenv;
	sig = Ringz.ar(sig, freq, decayenv);
	Out.ar(0, PanX.ar(2, sig, pan.linlin(-1, 1, 0, 1)));
}).add;



)

Synth(\pulsefiltersynth, [\freq, 55, \attack, 0.5, \dur, 5, \amp, 0.6, \pan, 0, \filterattack, 1, \startcutoff, 55, \peakcutoff, 7000]);
Synth(\sawfiltersynth, [\freq, 110, \attack, 0.5, \dur, 5, \amp, 0.6, \pan, 0, \filterattack, 1, \startcutoff, 110, \peakcutoff, 7000]);
Synth(\whitenoisemoogfilter, [\attack, 10, \dur, 15, \amp, 0.3, \pan, 0, \filterattack, 8, \startcutoff, 200, \peakcutoff, 7000, \filtergain, 3]);

Synth(\noiseringzfilter, [\freq, 2000, \attack, 10, \dur, 15, \amp, 0.05, \pan, 0, \decayattack, 8, \startdecay, 0.0001, \peakdecay, 0.01, \decaycurve, 0]);

{ Ringz.ar(WhiteNoise.ar(0.005), 2000, 0.5) }.play
{ Resonz.ar(WhiteNoise.ar(0.5), 2000, XLine.kr(1, 0.001, 8)) }.play

// Subtractive with Ringz and Resonz
/*
"http://doc.sccode.org/Classes/Ringz.html"
"http://doc.sccode.org/Classes/Resonz.html"
*/

/*
Look at Lag
"https://sccode.org/1-4UQ"
*/

(
var pulse, saw;

pulse = Pbind(\instrument, \pulsefiltersynth,
	\midinote, Pseq([45, 45, 40, 43], inf),
	\attack, 0.02,
	\pan, 0,
	\dur, 0.125,
	\amp, Pxrand([0.2, 0.3, 0.1], inf),
	\filterattack, 0.02,
	\startcutoff, 45,
	\peakcutoff, 7000);

saw = Pbind(\instrument, \sawfiltersynth,
	\midinote, Pseq([57, 57, 52, 55], inf),
	\attack, 0.02,
	\pan, 0,
	\dur, 0.125,
	\amp, Pxrand([0.2, 0.3, 0.1], inf),
	\filterattack, 0.02,
	\startcutoff, 45,
	\peakcutoff, 7000);
Ppar([pulse, saw]).play;
)


(
var pulse1, pulse2;

pulse1 = Pbind(\instrument, \doublesawfiltersynth,
	\midinote, Pxrand([57, 65, 64, 67], inf),
	\attack, 1,
	\pan, Pxrand([-0.75, -0.25, 0.25, 0.75], inf),
	\dur, Pxrand([8, 5, 13], inf),
	\amp, Pxrand([0.2, 0.3, 0.1], inf),
	\filterattack, 2,
	\startcutoff, 45,
	\filtercurve, 0,
	\peakcutoff, 3000);

pulse2 = Pbind(\instrument, \doublesawfiltersynth,
	\midinote, Pxrand([45, 40, 43], inf),
	\attack, 2,
	\pan, Pxrand([-0.5, 0, 0.5], inf),
	\dur, Pxrand([13, 21], inf),
	\amp, Pxrand([0.2, 0.3, 0.1], inf),
	\filterattack, 6,
	\startcutoff, 45,
	\filtercurve, 0,
	\peakcutoff, 2000);

Ppar([pulse1, pulse2]).play;
)

/*
Bladerunner main theme
"https://www.youtube.com/watch?time_continue=103&v=lTpIqVw37ys"
Start with a saw wave
Two saw wave osc slightly detuned

"http://www.musictech.net/2016/08/the-7-deadly-synth-sounds/"
"https://www.youtube.com/watch?v=iCdD7SabVb8"

*/

/*
"https://composerprogrammer.com/teaching/supercollider/sctutorial/11.2 Analogue Modelling.html"

Many slightly detuned versions
First version could use Mix
"http://doc.sccode.org/Classes/Mix.html"
The problem is that it wont spread the pan.

"http://doc.sccode.org/Classes/SynthDef.html"
Use Rand NOT rrand

*/

(
SynthDef(\detunepulsefiltersynth, {
	arg dur = 1, freq = 440, amp = 0.5, attack = 0.02, pan = 0,
	filterattack = 0.02, startcutoff = 440, peakcutoff = 440, filtergain = 2, ampcurve = -4, filtercurve = -4;
	var env, sig, filterenv, numdetune=6;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp / numdetune, curve:ampcurve), doneAction:2);
	filterenv = EnvGen.kr(Env.perc(filterattack, dur - filterattack, 1, curve:filtercurve), doneAction:2).linlin(0, 1, startcutoff, peakcutoff);

	sig = Mix(Pulse.ar(Array.rand(numdetune,0.99,1.01) * freq)) * env;

	sig = MoogFF.ar(sig, filterenv, filtergain);
	Out.ar(0, PanX.ar(2, sig, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\detunesawfiltersynth, {
	arg dur = 1, freq = 440, amp = 0.5, attack = 0.02, pan = 0,
	filterattack = 0.02, startcutoff = 440, peakcutoff = 440, filtergain = 2, ampcurve = -4, filtercurve = -4;
	var env, sig, filterenv, numdetune=6;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp / numdetune, curve:ampcurve), doneAction:2);
	filterenv = EnvGen.kr(Env.perc(filterattack, dur - filterattack, 1, curve:filtercurve), doneAction:2).linlin(0, 1, startcutoff, peakcutoff);

	sig = Mix(LFSaw.ar(Array.rand(numdetune,0.99,1.01) * freq)) * env;

	sig = MoogFF.ar(sig, filterenv, filtergain);
	Out.ar(0, PanX.ar(2, sig, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\detunetrifiltersynth, {
	arg dur = 1, freq = 440, amp = 0.5, attack = 0.02, pan = 0,
	filterattack = 0.02, startcutoff = 440, peakcutoff = 440, ampcurve = -4, filtercurve = -4;
	var env, sig, filterenv, numdetune=6;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp / numdetune, curve:ampcurve), doneAction:2);
	filterenv = EnvGen.kr(Env.perc(filterattack, dur - filterattack, 1, curve:filtercurve), doneAction:2).linlin(0, 1, startcutoff, peakcutoff);

	sig = Mix(LFTri.ar(Array.rand(numdetune,0.99,1.01) * freq)) * env;

	sig = MoogFF.ar(sig, filterenv);
	Out.ar(0, PanX.ar(2, sig, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\detunesinefiltersynth, {
	arg dur = 1, freq = 440, amp = 0.5, attack = 0.02, pan = 0, ampcurve = -4;
	var env, sig, numdetune=6;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp / numdetune, curve:ampcurve), doneAction:2);

	sig = Mix(SinOsc.ar(Array.rand(numdetune, 0.99, 1.01) * freq)) * env;

	Out.ar(0, PanX.ar(2, sig, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\detuneringmodulation, {
	arg dur = 1, carrierfreq = 440, modulatorfreq = 440, amp = 0.5, attack = 0.02, pan = 0, ampcurve = -4;
	var env, numdetune=6, carriersig, modulatorsig, sig;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp / numdetune, curve:ampcurve), doneAction:2);

	sig = Mix.fill(numdetune, {
		SinOsc.ar(Rand(0.99, 1.01) * carrierfreq, 0, 0.5) * SinOsc.ar(Rand(0.99, 1.01) * modulatorfreq, 0, 0.5)
	});
	Out.ar(0, PanX.ar(2, sig * env, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\detuneringpulsemodulation, {
	arg dur = 1, carrierfreq = 440, modulatorfreq = 440, amp = 0.5, attack = 0.02, pan = 0, ampcurve = -4;
	var env, numdetune=6, carriersig, modulatorsig, sig;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp / numdetune, curve:ampcurve), doneAction:2);

	sig = Mix.fill(numdetune, {
		Pulse.ar(Rand(0.99, 1.01) * carrierfreq) * SinOsc.ar(Rand(0.99, 1.01) * modulatorfreq, 0, 0.5)
	});
	Out.ar(0, PanX.ar(2, sig * env, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\detuneringsawmodulation, {
	arg dur = 1, carrierfreq = 440, modulatorfreq = 440, amp = 0.5, attack = 0.02, pan = 0, ampcurve = -4;
	var env, numdetune=6, carriersig, modulatorsig, sig;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp / numdetune, curve:ampcurve), doneAction:2);

	sig = Mix.fill(numdetune, {
		LFSaw.ar(Rand(0.99, 1.01) * carrierfreq) * SinOsc.ar(Rand(0.99, 1.01) * modulatorfreq, 0, 0.5)
	});
	Out.ar(0, PanX.ar(2, sig * env, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\detuneringtrimodulation, {
	arg dur = 1, carrierfreq = 440, modulatorfreq = 440, amp = 0.5, attack = 0.02, pan = 0, ampcurve = -4;
	var env, numdetune=6, carriersig, modulatorsig, sig;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp / numdetune, curve:ampcurve), doneAction:2);

	sig = Mix.fill(numdetune, {
		LFTri.ar(Rand(0.99, 1.01) * carrierfreq) * SinOsc.ar(Rand(0.99, 1.01) * modulatorfreq, 0, 0.5)
	});
	Out.ar(0, PanX.ar(2, sig * env, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\detuneammodulation, {
	arg dur = 1, carrierfreq = 440, modulatorfreq = 440, amp = 0.5, attack = 0.02, pan = 0, ampcurve = -4;
	var env, numdetune=6, carriersig, modulatorsig, sig;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp / numdetune, curve:ampcurve), doneAction:2);

	sig = Mix.fill(numdetune, {
		SinOsc.ar(Rand(0.99, 1.01) * carrierfreq, 0, 0.5) * SinOsc.ar(Rand(0.99, 1.01) * modulatorfreq, 0, 0.25, 0.25)
	});
	Out.ar(0, PanX.ar(2, sig * env, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\detuneampulsemodulation, {
	arg dur = 1, carrierfreq = 440, modulatorfreq = 440, amp = 0.5, attack = 0.02, pan = 0, ampcurve = -4;
	var env, numdetune=6, carriersig, modulatorsig, sig;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp / numdetune, curve:ampcurve), doneAction:2);

	sig = Mix.fill(numdetune, {
		Pulse.ar(Rand(0.99, 1.01) * carrierfreq) * SinOsc.ar(Rand(0.99, 1.01) * modulatorfreq, 0, 0.25, 0.25)
	});
	Out.ar(0, PanX.ar(2, sig * env, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\detuneamsawmodulation, {
	arg dur = 1, carrierfreq = 440, modulatorfreq = 440, amp = 0.5, attack = 0.02, pan = 0, ampcurve = -4;
	var env, numdetune=6, carriersig, modulatorsig, sig;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp / numdetune, curve:ampcurve), doneAction:2);

	sig = Mix.fill(numdetune, {
		LFSaw.ar(Rand(0.99, 1.01) * carrierfreq) * SinOsc.ar(Rand(0.99, 1.01) * modulatorfreq, 0, 0.25, 0.25)
	});
	Out.ar(0, PanX.ar(2, sig * env, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\detuneamtrimodulation, {
	arg dur = 1, carrierfreq = 440, modulatorfreq = 440, amp = 0.5, attack = 0.02, pan = 0, ampcurve = -4;
	var env, numdetune=6, carriersig, modulatorsig, sig;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp / numdetune, curve:ampcurve), doneAction:2);

	sig = Mix.fill(numdetune, {
		LFTri.ar(Rand(0.99, 1.01) * carrierfreq) * SinOsc.ar(Rand(0.99, 1.01) * modulatorfreq, 0, 0.25, 0.25)
	});
	Out.ar(0, PanX.ar(2, sig * env, pan.linlin(-1, 1, 0, 1)));
}).add;


SynthDef(\detunefmmodulation, {
	arg dur = 1, carrierfreq = 440, modulatorfreq = 440, modulatorindex = 1, amp = 0.5, attack = 0.02, pan = 0, ampcurve = -4;
	var env, numdetune=6, carriersig, modulatorsig, sig;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp / numdetune, curve:ampcurve), doneAction:2);

	sig = Mix.fill(numdetune, {
		SinOsc.ar((Rand(0.99, 1.01) * carrierfreq) + (modulatorindex * modulatorfreq * SinOsc.ar(Rand(0.99, 1.01) * modulatorfreq)), 0, 0.25)
	});
	Out.ar(0, PanX.ar(2, sig * env, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\detunepmmodulation, {
	arg dur = 1, carrierfreq = 440, modulatorfreq = 440, modulatorindex = 1, amp = 0.5, attack = 0.02, pan = 0, ampcurve = -4;
	var env, numdetune=6, carriersig, modulatorsig, sig, conversion = 2pi/(s.sampleRate);
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp / numdetune, curve:ampcurve), doneAction:2);

	sig = Mix.fill(numdetune, {
		SinOsc.ar(
			Rand(0.99, 1.01) * carrierfreq,
			Rand(0.99, 1.01) * modulatorfreq * modulatorindex * conversion * SinOsc.ar(Rand(0.99, 1.01) * modulatorfreq), 0.25);
	});
	Out.ar(0, PanX.ar(2, sig * env, pan.linlin(-1, 1, 0, 1)));
}).add;
)

Synth(\detunesawfiltersynth, [\freq, 440, \attack, 1, \dur, 5, \amp, 0.6, \pan, 0, \filterattack, 1, \startcutoff, 55, \peakcutoff, 4000]);

Synth(\detunetrifiltersynth, [\freq, 880, \attack, 1, \dur, 5, \amp, 0.6, \pan, 0, \filterattack, 1, \startcutoff, 880, \peakcutoff, 8000]);

Synth(\detunesinefiltersynth, [\freq, 110, \attack, 1, \dur, 5, \amp, 0.6, \pan, 0]);

Synth(\detunepulsefiltersynth, [\freq, 440, \attack, 1, \dur, 5, \amp, 0.6, \pan, 0, \filterattack, 1, \startcutoff, 880, \peakcutoff, 8000]);

Synth(\detunesawfiltersynth, [\freq, 110, \attack, 0.05, \dur, 0.5, \amp, 0.6, \pan, 0, \filterattack, 1, \startcutoff, 110, \peakcutoff, 7000]);

Synth(\detuneringmodulation, [\carrierfreq, 56.midicps, \modulatorfreq, 61.midicps, \attack, 0.5, \dur, 5, \amp, 0.6, \pan, 0]);

Synth(\detuneringpulsemodulation, [\carrierfreq, 56.midicps, \modulatorfreq, 61.midicps, \attack, 0.5, \dur, 5, \amp, 0.6, \pan, 0]);

Synth(\detuneringsawmodulation, [\carrierfreq, 56.midicps, \modulatorfreq, 61.midicps, \attack, 0.5, \dur, 5, \amp, 0.6, \pan, 0]);

Synth(\detuneringtrimodulation, [\carrierfreq, 56.midicps, \modulatorfreq, 61.midicps, \attack, 0.5, \dur, 5, \amp, 0.6, \pan, 0]);

Synth(\detunesinefiltersynth, [\freq, 61.midicps, \attack, 1, \dur, 5, \amp, 0.6, \pan, 0]);

Synth(\detuneammodulation, [\carrierfreq, 56.midicps, \modulatorfreq, 61.midicps, \attack, 0.5, \dur, 5, \amp, 0.6, \pan, 0]);

Synth(\detuneampulsemodulation, [\carrierfreq, 56.midicps, \modulatorfreq, 61.midicps, \attack, 0.5, \dur, 5, \amp, 0.6, \pan, 0]);

Synth(\detuneamsawmodulation, [\carrierfreq, 56.midicps, \modulatorfreq, 61.midicps, \attack, 0.5, \dur, 5, \amp, 0.6, \pan, 0]);

Synth(\detuneamtrimodulation, [\carrierfreq, 56.midicps, \modulatorfreq, 61.midicps, \attack, 0.5, \dur, 5, \amp, 0.6, \pan, 0]);

Synth(\detunefmmodulation, [\carrierfreq, 56.midicps, \modulatorfreq, 61.midicps, \modulatorindex, 9, \attack, 0.5, \dur, 5, \amp, 0.6, \pan, 0]);

Synth(\detunefmmodulation, [\carrierfreq, 56.midicps, \modulatorfreq, 44.midicps, \modulatorindex, 9, \attack, 0.5, \dur, 5, \amp, 0.6, \pan, 0]);

Synth(\detunepmmodulation, [\carrierfreq, 56.midicps, \modulatorfreq, 44.midicps, \modulatorindex, 2, \attack, 0.5, \dur, 5, \amp, 0.6, \pan, 0]);


(
var pulse, saw;

pulse = Pbind(\instrument, \detunepulsefiltersynth,
	\midinote, Pseq([45, 45, 40, 43], inf),
	\attack, 0.02,
	\pan, 0,
	\dur, 0.125,
	\amp, Pxrand([0.2, 0.3, 0.1], inf),
	\filterattack, 0.02,
	\startcutoff, 45,
	\peakcutoff, 7000);

saw = Pbind(\instrument, \detunesawfiltersynth,
	\midinote, Pseq([57, 57, 52, 55], inf),
	\attack, 0.02,
	\pan, 0,
	\dur, 0.125,
	\amp, Pxrand([0.2, 0.3, 0.1], inf),
	\filterattack, 0.02,
	\startcutoff, 45,
	\peakcutoff, 7000);
Ppar([pulse, saw]).play;
)

(
var pulse1, pulse2;

pulse1 = Pbind(\instrument, \detunesawfiltersynth,
	\midinote, Pxrand([57, 65, 64, 67], inf),
	\attack, 1,
	\pan, Pxrand([-0.75, -0.25, 0.25, 0.75], inf),
	\dur, Pxrand([8, 5, 13], inf),
	\amp, Pxrand([0.2, 0.3, 0.1], inf),
	\filterattack, 2,
	\startcutoff, 45,
	\filtercurve, 0,
	\peakcutoff, 3000);

pulse2 = Pbind(\instrument, \detunesawfiltersynth,
	\midinote, Pxrand([45, 40, 43], inf),
	\attack, 2,
	\pan, Pxrand([-0.5, 0, 0.5], inf),
	\dur, Pxrand([13, 21], inf),
	\amp, Pxrand([0.2, 0.3, 0.1], inf),
	\filterattack, 6,
	\startcutoff, 45,
	\filtercurve, 0,
	\peakcutoff, 2000);

Ppar([pulse1, pulse2]).play;
)


/*
"https://www.youtube.com/watch?v=Dc6cwZFuO0U"
Convolution revereb

"http://jackaudio.org/"
"Ardour DAW"
*/


/*
"http://doc.sccode.org/Tutorials/Mark_Polishook_tutorial/17_Delays_reverbs.html"
"https://composerprogrammer.com/teaching/supercollider/sctutorial/6.4 Effects 1.html"
*/

(
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

// Look at mono vs stereo
SynthDef(\phaser, {
	arg in = 0, freq = 0.2, wet = 0.2, amount = 0.01;
	var sig, input;

	input = In.ar(in,2);

	sig = AllpassN.ar(input, amount * 2, SinOsc.kr(freq, 0, amount, amount));
	Out.ar(0, (input * (1 - wet)) + (sig * wet));
}).add;

SynthDef(\flanger, {
	arg in = 0, freq = 0.1, feecback = 0.1;
	var input, effect;

	input = In.ar(in, 2);
	input = input + LocalIn.ar(2);
	effect = DelayN.ar(input, 0.02, SinOsc.kr(freq, 0, 0.005, 0.005));
	LocalOut.ar(feecback * effect);
	Out.ar(0, effect);
}).add;


//"http://ecmc.rochester.edu/ecmc/docs/supercollider/scbook/Ch21_Interface_Investigations/ixi SC tutorial/ixi_SC_tutorial_12.html"
/*SynthDef(\chorus, {
	arg in = 0, predelay = 0.08, speed = 0.05, depth = 0.1, ph_diff = 0.5;
	var input, effect, modulators, numdelays = 12;
	input = In.ar(in, 2) * numdelays.reciprocal;
	modulators = Array.fill(numdelays, {
		arg i;
		LFPar.kr(speed * Rand(0.94, 1.06), ph_diff * i, depth, predelay);
	});
	effect = Mix.new(DelayC.ar(input, 0.5, modulators));
	Out.ar(0, PanX.ar(2, effect, 0.5));

}).add;
*/
/*

SynthDef(\chorus, { arg inbus=10, outbus=0, predelay=0.08, speed=0.05, depth=0.1, ph_diff=0.5;
	var in, sig, modulators, numDelays = 12;
	in = In.ar(inbus, 1) * numDelays.reciprocal;
	modulators = Array.fill(numDelays, {arg i;
      	LFPar.kr(speed * rrand(0.94, 1.06), ph_diff * i, depth, predelay);});
	sig = DelayC.ar(in, 0.5, modulators);
	sig = sig.sum; //Mix(sig);
	Out.ar(outbus, sig!2); // output in stereo
}).add
*/

//"https://composerprogrammer.com/teaching/supercollider/sctutorial/6.4 Effects 1.html"
SynthDef(\chorus, {
	arg in = 0;
	var input, effect, numDelays = 10;

	//input = In.ar(in, 2) * numDelays.reciprocal;
	input = In.ar(in, 2);

	effect = Mix.fill(numDelays, {
		var maxdelaytime = Rand(0.01, 0.03);
		DelayC.ar(input, maxdelaytime, LFNoise1.kr(Rand(5, 10), 0.01, 0.02));
	});
	Out.ar(0, PanX.ar(2, effect, 0.5));
}).add;

// "http://www.dewdrop-world.net/sc3/tutorials/index.php?id=5"

SynthDef(\chorus2, {
	arg in = 0, predelay = 0.08, speed = 0.05, depth = 0.1, ph_diff = 0.5, amp = 0.5;
	var input, sig, numdelays = 10, modulators;

	input = In.ar(in, 2) * amp;
	modulators = Array.fill(numdelays, {
		arg i;
		SinOsc.kr(speed * Rand(0.9, 1.1), ph_diff * i, depth, predelay);
	});
	sig = DelayL.ar(input, 0.5, modulators);
	Out.ar(0, PanX.ar(2, sig, 0.5));

}).add;

)

Synth(\reverb, [\combdelay, 0.1, \combdecay, 15, \allpassdelay, 0.05, \allpassdecay, 1]);

Synth(\reverb, [\wet, 0.1, \combdelay, 0.05, \combdecay, 0.5, \allpassdecay, 0.5, \allpassdelay, 0.05]);

Synth(\phaser, [\mix, 1, \freq, 0.05, \amount, 0.005, \wet, 0.9]);
Synth(\flanger);
Synth(\chorus);
Synth(\chorus2, [\predelay, 0.08, \speed, 0.05, \depth, 0.1, \ph_diff, 0.5]);

Synth(\detunepulsefiltersynth, [\freq, 55, \attack, 0.05, \dur, 0.5, \amp, 0.6, \pan, 0, \filterattack, 1, \startcutoff, 110, \peakcutoff, 7000]);
Synth(\detunesawfiltersynth, [\freq, 110, \attack, 0.05, \dur, 0.5, \amp, 0.6, \pan, 0, \filterattack, 1, \startcutoff, 110, \peakcutoff, 7000]);
Synth(\detunepulsefiltersynth, [\freq, 220, \attack, 0.05, \dur, 0.5, \amp, 0.6, \pan, 0, \filterattack, 1, \startcutoff, 110, \peakcutoff, 7000]);

Synth(\detunefmmodulation, [\carrierfreq, 56.midicps, \modulatorfreq, 61.midicps, \modulatorindex, 9, \attack, 0.5, \dur, 5, \amp, 0.6, \pan, 0]);

(
SynthDef(\moogfilter, {
	arg in = 0, freqspeed = 0.001, startFreq = 100, endFreq = 4000, gainspeed = 0.001, startgain = 3.5, endgain = 3.5;
	var input, sig;
	input = In.ar(in, 2);
	sig = MoogFF.ar(input,
		SinOsc.kr(freqspeed).range(startFreq, endFreq),
		SinOsc.kr(gainspeed).range(startgain, endgain));
	ReplaceOut.ar(0, sig);
}).add;

)

Synth(\moogfilter, [\freqspeed, 0.1, \startFreq, 500, \gainspeed, 0.1, \startgain, 1, \endgain, 3.5]);

(
SynthDef(\whitenoise, {
	arg dur = 1, amp = 0.5, attack = 0.02, pan = 0;
	var env, sig;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp), doneAction:2);
	sig = WhiteNoise.ar() * env;
	Out.ar(0, PanX.ar(2, sig, pan.linlin(-1, 1, 0, 1)));
}).add;

SynthDef(\pinknoise, {
	arg dur = 1, amp = 0.5, attack = 0.02, pan = 0;
	var env, sig;
	env = EnvGen.kr(Env.perc(attack, dur - attack, amp), doneAction:2);
	sig = PinkNoise.ar() * env;
	Out.ar(0, PanX.ar(2, sig, pan.linlin(-1, 1, 0, 1)));
}).add;

)

Synth(\moogfilter, [\freqspeed, 0.1, \startFreq, 500, \gain, 2]);
Synth(\moogfilter, [\freqspeed, 0.1, \startFreq, 500, \gainspeed, 0.3, \startgain, 1, \endgain, 3.5]);
Synth(\whitenoise, [\dur, 8, \attack, 2]);
Synth(\pinknoise, [\dur, 8, \attack, 2]);

/*
Ring modulation
*/

/*
Flanger
chorus
Phaser

"https://www.youtube.com/watch?v=zLJ1gcw0xbc"


arp
a  c  e  a+  e  c  a  e  c
81 84 88 93  88 84 81 88 84
a  c  f  a+  f  c  a  f  c
81 84 89 93 89  84 81 89 84

*/

(
var pulse, saw;

pulse = Pbind(\instrument, \detunepulsefiltersynth,
	\midinote, Pseq([81, 84, 88, 93,  88, 84, 81, 88, 84, 81, 84, 89, 93, 89,  84, 81, 89, 84].collect { | i | i - 36};, inf),
	\attack, 0.02,
	\pan, Pxrand([-0.5, 0, 0.5], inf),
	\dur, 0.125,
	\amp, Pxrand([0.2, 0.3, 0.1], inf),
	\filterattack, 0.02,
	\startcutoff, 45,
	\peakcutoff, 7000);

saw = Pbind(\instrument, \detunesawfiltersynth,
	\midinote, Pseq([81, 84, 88, 93,  88, 84, 81, 88, 84, 81, 84, 89, 93, 89,  84, 81, 89, 84].collect { | i | i - 36};, inf),
	\attack, 0.02,
	\pan, Pxrand([-0.7, -0.3, 0.3, 0.7], inf),
	\dur, 0.125,
	\amp, Pxrand([0.2, 0.3, 0.1], inf),
	\filterattack, 0.02,
	\startcutoff, 45,
	\peakcutoff, 7000);

Ppar([pulse, saw]).play;
)


[81, 84, 88, 93,  88, 84, 81, 88, 84, 81, 84, 89, 93, 89,  84, 81, 89, 84,].collect { | i | i + 12};


// "https://composerprogrammer.com/teaching/supercollider/sctutorial/2.4 Modulation Synthesis.html"


(

SynthDef(\delay, {
	arg in = 0, delaytime = 0.2, decaytime = 1;
	var input, sig;

	input = In.ar(in, 2);
	sig = CombL.ar(input, delaytime, delaytime, decaytime);

	Out.ar(0, sig);
}).add;


)

Synth(\delay, [\delaytime, 0.5, \decaytime, 20]);
Synth(\delay, [\delaytime, 0.51, \decaytime, 20]);
Synth(\delay, [\delaytime, 0.49, \decaytime, 20]);

Synth(\detunepulsefiltersynth, [\freq, 60.midicps, \attack, 0.05, \dur, 0.5, \amp, 0.6, \pan, 0, \filterattack, 1, \startcutoff, 110, \peakcutoff, 7000]);
Synth(\detunepulsefiltersynth, [\freq, 64.midicps, \attack, 0.05, \dur, 0.5, \amp, 0.6, \pan, 0, \filterattack, 1, \startcutoff, 110, \peakcutoff, 7000]);
Synth(\detunepulsefiltersynth, [\freq, 63.midicps, \attack, 0.05, \dur, 0.5, \amp, 0.6, \pan, 0, \filterattack, 1, \startcutoff, 110, \peakcutoff, 7000]);
Synth(\detunepulsefiltersynth, [\freq, 65.midicps, \attack, 0.05, \dur, 0.5, \amp, 0.6, \pan, 0, \filterattack, 1, \startcutoff, 110, \peakcutoff, 7000]);


// Gardner reverb
"http://doc.sccode.org/Classes/DoubleNestedAllpassC.html"




// Eli notes


// Buchla?

{Pan2.ar(PinkNoise.ar(0.2),MouseX.kr(-1,1))}.play
{LinPan2.ar(PinkNoise.ar(0.2),MouseX.kr(-1,1))}.play

// Waveshaping?
// Granular, Grains? "https://www.youtube.com/watch?v=Qxqtc2bgDpw"

b = Buffer.alloc(s, 512, 1, { |buf| buf.chebyMsg([1,0,1,1,0,1])});

(
{
    Shaper.ar(
        b,
        SinOsc.ar(300, 0, Line.kr(0,1,6)),
        0.5
    )
}.scope;
)

b.free;

// A more generic FM synthesis

{SinOsc.ar(440 + (SinOsc.ar(220) * 5000))}.play

(
{
	var modulator, modFreq, modIndex, amp;
	modFreq = 440;
	amp = 1;
	modIndex = Line.kr(100, 2000, 5);
	modulator = SinOsc.ar(modFreq) * modIndex;

	SinOsc.ar(440 + modulator, 0, amp);
}.play;
)

/*
This way you will be able to have the freedom of different osc types
The freqbus (at audiorate) is a oscillator times a modIndex + the carrier freq.
1. Make a oscillator (sine, square, pulse etc) with the modFreq
SinOsc.ar(modFreq)
2. Multiply the modulator with the modIndex
3. Add the carrierFreq to the modulator.
4. Have that bus as the carrier freqbus
SinOsc.ar(freqBus)

val modFreq = staticControl(440f)
val modIndex = staticControl(2000)
val modOsc = sine(modFreq)
val carrierFreq = staticControl(440f)
val modulator = carrierFreq + (modOsc * modIndex)

val carrier = sine(freq = modulator)
*/


(
SynthDef(\fmSineOsc, {
	arg dur = 1, freqBus = 0, modBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq, mod;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(freqBus, 1);
	mod = In.ar(modBus, 1);

	sig = SinOsc.ar(freq + mod, mul:amp);
	Out.ar(out, sig);
}).add;

SynthDef(\fmPulseOsc, {
	arg dur = 1, freqBus = 0, modBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq, mod;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(freqBus, 1);
	mod = In.ar(modBus, 1);

	sig = Pulse.ar(freq + mod, mul:amp);
	Out.ar(out, sig);
}).add;

SynthDef(\fmSawOsc, {
	arg dur = 1, freqBus = 0, modBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq, mod;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(freqBus, 1);
	mod = In.ar(modBus, 1);

	sig = LFSaw.ar(freq + mod, mul:amp);
	Out.ar(out, sig);
}).add;

SynthDef(\fmTriangleOsc, {
	arg dur = 1, freqBus = 0, modBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq, mod;
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(freqBus, 1);
	mod = In.ar(modBus, 1);

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

SynthDef(\pmPulseOsc, {
	arg dur = 1, freqBus = 0, modBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq, mod, conversion= 2pi/(s.sampleRate);
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(freqBus, 1);
	mod = In.ar(modBus, 1);

	sig = Pulse.ar(freq, mod * conversion, mul:amp);
	Out.ar(out, sig);
}).add;

SynthDef(\pmSawOsc, {
	arg dur = 1, freqBus = 0, modBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq, mod, conversion= 2pi/(s.sampleRate);
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(freqBus, 1);
	mod = In.ar(modBus, 1);

	sig = LFSaw.ar(freq, mod * conversion, mul:amp);
	Out.ar(out, sig);
}).add;

SynthDef(\pmTriangleOsc, {
	arg dur = 1, freqBus = 0, modBus = 0, ampBus = 0, out = 0;
	var sig, amp, freq, mod, conversion= 2pi/(s.sampleRate);
	Line.kr(dur:dur, doneAction:2);
	amp = In.kr(ampBus, 1);
	freq = In.kr(freqBus, 1);
	mod = In.ar(modBus, 1);

	sig = LFTri.ar(freq, mod * conversion, mul:amp);
	Out.ar(out, sig);
}).add;
)

// Perhaps try to implement and look at these
// https://www.adsrsounds.com/fm8-tutorials/using-theory-behind-fm-synthesis-in-practice/
// https://www.reddit.com/r/edmproduction/comments/1bc0xb/fm_synthesis_techniques_introduction_and_harmonic/
// https://www.soundonsound.com/techniques/more-frequency-modulation
// https://www.tinyloops.com/doc/yamaha_dx7/algorithms.html
// https://yamahasynth.com/yamaha-synth-rss/basics-of-fm-synthesis
(
var dur = 13, modFreqBus = Bus.control(s, 1), modOut = Bus.audio(s, 1), modOscBus = Bus.audio(s, 1), modIndexBus = Bus.control(s, 1),
carrierFreqBus = Bus.control(s, 1), amp = Bus.control(s, 1), panBus = Bus.control(s, 1), sigOut = Bus.audio(s, 1), carrierOut = Bus.audio(s, 1);

Synth(\percControl, [\out, amp, \dur, dur, \attackTime, 2, \startValue, 0.001, \peakValue, 0.2, \curve, \lin]);
Synth(\lineControl, [\out, modFreqBus, \dur, dur, \startValue, 45.midicps, \endValue, 55.midicps]);
Synth(\percControl, [\out, modIndexBus, \dur, dur, \attackTime, 8, \startValue, 5000, \peakValue, 30, \curve, \lin]);
Synth(\lineControl, [\out, carrierFreqBus, \dur, dur, \startValue, 50.midicps, \endValue, 45.midicps]);
Synth(\staticControl, [\out, panBus, \dur, dur, \value, 0]);
Synth(\sineOsc, [\dur, dur, \out, modOut, \freqBus, modFreqBus, \ampBus, modIndexBus], addAction:\addToTail);
Synth(\fmSineOsc, [\dur, dur, \out, sigOut, \freqBus, carrierFreqBus, \modBus, modOut, \ampBus, amp], addAction:\addToTail);
Synth(\pan, [\in, sigOut, \panBus, panBus, \out, 0, \dur, dur], addAction:\addToTail);
)



/*
We should probably make this different. modFreq * modIndex * conversion * SinOsc.ar(modFreq)
*/
(
var dur = 13, modFreqBus = Bus.control(s, 1), modOut = Bus.audio(s, 1), modOscBus = Bus.audio(s, 1), modIndexBus = Bus.control(s, 1),
carrierFreqBus = Bus.control(s, 1), amp = Bus.control(s, 1), panBus = Bus.control(s, 1), sigOut = Bus.audio(s, 1), carrierOut = Bus.audio(s, 1);

Synth(\percControl, [\out, amp, \dur, dur, \attackTime, 2, \startValue, 0.001, \peakValue, 0.1, \curve, \lin]);
Synth(\staticControl, [\out, modFreqBus, \dur, dur, \value, 50.midicps * 10]);
Synth(\percControl, [\out, modIndexBus, \dur, dur, \attackTime, 2, \startValue, 10, \peakValue, 4000, \curve, \lin]);
Synth(\staticControl, [\out, carrierFreqBus, \dur, dur, \value, 50.midicps]);
Synth(\staticControl, [\out, panBus, \dur, dur, \value, 0]);
Synth(\sineOsc, [\dur, dur, \out, modOut, \freqBus, modFreqBus, \ampBus, modIndexBus], addAction:\addToTail);
Synth(\pmSineOsc, [\dur, dur, \out, sigOut, \freqBus, carrierFreqBus, \modBus, modOut, \ampBus, amp], addAction:\addToTail);
Synth(\pan, [\in, sigOut, \panBus, panBus, \out, 0, \dur, dur], addAction:\addToTail);
)


(

var w, carrfreqslider, modfreqslider, modindexslider, synth;
var conversion= 2pi/(s.sampleRate); //needed to avoid phase being adjusted too wildly

w=Window("phase modulation via modulation index", Rect(100, 400, 400, 300));
w.view.decorator = FlowLayout(w.view.bounds);

synth= {arg carrfreq=440, modfreq=1, modindex=0;
SinOsc.ar(carrfreq, ( (modfreq * modindex) * conversion * SinOsc.ar(modfreq)), 0.25)
}.scope;

carrfreqslider= EZSlider(w, 300@50, "carrfreq", ControlSpec(20, 5000, 'exponential', 10, 440), {|ez|  synth.set(\carrfreq, ez.value)});
w.view.decorator.nextLine;

modfreqslider= EZSlider(w, 300@50, "modfreq", ControlSpec(1, 5000, 'exponential', 1, 1), {|ez|  synth.set(\modfreq, ez.value)});
w.view.decorator.nextLine;

//bigger range since adjusting phase directly and not frequency
modindexslider= EZSlider(w, 300@50, "modindex", ControlSpec(0.0, 100, 'linear', 0.01, 0.0), {|ez|  synth.set(\modindex, ez.value)});
w.front;

)


(
{SinOsc.ar(0.5).linlin(-1, 1, 300, 3000)}.plot(4);
)


// Whitenoises advice on mixing. https://www.youtube.com/watch?v=Taylf4EQqhI
// Reverb, compressor and limiter
// Other tips https://www.youtube.com/watch?v=XD2swcN0h8c
// Record into reaper https://www.youtube.com/watch?v=V8BTR3BZoHg
