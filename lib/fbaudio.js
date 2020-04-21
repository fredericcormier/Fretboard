function FBAudio() {
    this.tempo = 90;
    this.current_note = 0;
    Tone.Transport.bpm.value = this.tempo;
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
        "F5": "./sounds/77-F5.mp3",
        "G#5": "./sounds/80-GS5.mp3"
    }).toMaster();

    Object.freeze(this.sampler);

    var that = this;
    this.part = new Tone.Part(function (time, event) {
        that.sampler.triggerAttackRelease(event.note, event.dur);
        // Logger.debug("sound loaded:" + that.sampler.loaded);
    }, []);
}



FBAudio.prototype = {
    start: function () {
        this.part.loop = 99999;
        this.part.start(0);
        Tone.Transport.start('+0.1');
    },

    stop: function () {
        Tone.Transport.stop();
        Tone.Transport.cancel(0);
        Tone.Transport.position = "00:00:00";
    },

    notesHaveChanged: function (newNotes) {
        this.runLength = newNotes.length;
        this.part.loopEnd = '16n * ' + (this.runLength);
        for (var i = 0; i < this.runLength; i++) {
            this.part.at('16n *' + i, {
                time: '16n *' + i,
                note: newNotes[i],
                dur: '16n'
            });
        }
    }
};