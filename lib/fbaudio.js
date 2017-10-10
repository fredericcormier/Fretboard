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
    this.amsynth = new Tone.AMSynth(amSettings).toMaster();
    this.fmsynth = new Tone.FMSynth(fmSettings).toMaster();
    Object.freeze(this.noisesynth);
    Object.freeze(this.amsynth);
    Object.freeze(this.fmsynth);
    this.noisesynth.volume.value = -30;
    // We pass an empty array to the callback because the notes
    // are scheduled in the notesHaveChanged function
    var that = this;
    this.part = new Tone.Part(function(time, event) {
        that.amsynth.triggerAttackRelease(event.note, event.dur, time);
        that.fmsynth.triggerAttackRelease(event.note, event.dur, time);
        that.noisesynth.triggerAttackRelease(event.dur, time);
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
