var amSettings = {
    "harmonicity": 1,
    "detune": 3,
    "oscillator": {
        "type": "sine"
    },
    "envelope": {
        "attack": 0.01,
        "decay": 0.5,
        "sustain": 0.01,
        "release": 0.01
    },
    "modulation": {
        "type": "triangle"
    },
    "modulationEnvelope": {
        "attack": 0.01,
        "decay": 0.5,
        "sustain": 0.01,
        "release": 0.01,
    }
};

var fmSettings = {
    "harmonicity": 8,
    "detune": 0,
    "oscillator": {
        "type": "sine"
    },
    "envelope": {
        "attack": 0.01,
        "decay": 0.5,
        "sustain": 0.01,
        "release": 0.01
    },
    "modulation": {
        "type": "triangle"
    },
    "modulationEnvelope": {
        "attack": 0.01,
        "decay": 0.55,
        "sustain": 0.01,
        "release": 0.01,
    }
};



function FBAudio() {
    this.tempo = 120;
    this.current_note = 0;
    Tone.Transport.bpm.value = this.tempo;
    this.noisesynth = new Tone.NoiseSynth().toMaster();
    // this.amsynth = new Tone.AMSynth(amSettings).toMaster();
    // this.fmsynth = new Tone.FMSynth(fmSettings).toMaster();
    Object.freeze(this.noisesynth);
    // Object.freeze(this.amsynth);
    // Object.freeze(this.fmsynth);
    this.noisesynth.volume.value = -30;

    // We pass an empty array to the callback because the notes
    // are scheduled in the notesHaveChanged function
    this.sampler = new Tone.Sampler({
        "D1": "./sounds/26-D1.mp3",
        "F1": "./sounds/29-F1.mp3",
        "G#1": "./sounds/32-GS1.mp3",
        "B1": "./sounds/35-B1.mp3",
        "D2": "./sounds/38-D2.mp3",
        "F2": "./sounds/41-F2.mp3",
        "G#2": "./sounds/44-GS2.mp3",
        "B2": "./sounds/47-B2.mp3",
        "D3": "./sounds/50-D3.mp3",
        "F3": "./sounds/53-F3.mp3",
        "G#3": "./sounds/56-GS3.mp3",
        "B3": "./sounds/59-B3.mp3",
        "D4": "./sounds/62-D4.mp3",
        "F4": "./sounds/65-F4.mp3",
        "G#4": "./sounds/68-GS4.mp3",
        "B4": "./sounds/71-B4.mp3",
        "D5": "./sounds/74-D5.mp3",
        "77": "./sounds/77-F5.mp3",
        "G#5": "./sounds/80-GS5.mp3",
    }).toMaster();
    var that = this;
    this.part = new Tone.Part(function(time, event) {
        that.sampler.triggerAttackRelease(event.note, event.dur);
        // that.amsynth.triggerAttackRelease(event.note, event.dur, time);
        // that.fmsynth.triggerAttackRelease(event.note, event.dur, time);
        // that.noisesynth.triggerAttackRelease(event.dur, time);
        // Logger.debug("sound loaded:" + that.sampler.loaded);
    }, []);
}





FBAudio.prototype = {
    start: function() {
        this.part.loop = 99999;
        this.part.start(0);
        Tone.Transport.start('+0.1');
    },

    stop: function() {
        Tone.Transport.stop();
        Tone.Transport.cancel(0);
        Tone.Transport.position = "00:00:00";
    },

    notesHaveChanged: function(newNotes) {
        this.runLength = newNotes.length;
        this.part.loopEnd = '16n * ' + (this.runLength);
        for (var i = 0; i < this.runLength; i++) {
            this.part.at('16n *' + i, {
                time: '16n *' + i,
                note: newNotes[i],
                dur: '16n'
            });
        }
        // Logger.debug('This run is ' + this.runLength + ' notes long');

    }
};
