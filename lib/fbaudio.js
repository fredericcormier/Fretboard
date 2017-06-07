var ammySettings = {
    "harmonicity": 4,
    "detune": 0,
    "oscillator": {
        "type": "square"
    },
    "envelope": {
        "attack": 0.01,
        "decay": 0.5,
        "sustain": 0.01,
        "release": 0.01
    },
    "modulation": {
        "type": "square"
    },
    "modulationEnvelope": {
        "attack": 0.01,
        "decay": 0.5,
        "sustain": 0.01,
        "release": 0.01,
    }
};


var notes = ["C4", "D4", "E4", "F4", "G4", "A4", "B4", "C5"];


function FBAudio() {
    this.tempo = 150;
    this.current_note = 0;
    Tone.Transport.bpm.value = this.tempo;
}

FBAudio.prototype = {
    start: function() {
        if (this.fbsynth === undefined) {
            this.fbsynth = new Tone.AMSynth(ammySettings).toMaster();
        }
        this.play();
        Tone.Transport.start('+0.1');
    },
    stop: function() {
        Tone.Transport.stop();
        Tone.Transport.cancel(0);
        // this.fbsynth.dispose();
        this.fbsynth = undefined;
    },
    play: function() {
        var that = this;
        Tone.Transport.scheduleRepeat(function(time) {
            var note = notes[that.current_note % notes.length];
            that.fbsynth.triggerAttackRelease(note, '16n', time);
            that.current_note++;
        }, '16n');
    }
};
