window.onload = () => {
  document.getElementById("audio_file").onchange = function(){
    const files = this.files;
    const reader = new FileReader();
    reader.onload = (e) => {
      const result = e.target.result;
      const samples = result.trim().split(" ").map(r => parseInt(r) / 2**16);
      const context = new AudioContext({sampleRate: 48000});
      const audioBuffer = context.createBuffer(1, samples.length, context.sampleRate);
      audioBuffer.copyToChannel(Float32Array.from(samples), 0);
      const source = context.createBufferSource();
      source.buffer = audioBuffer;
      source.connect(context.destination);
      source.start();
    }
    reader.readAsText(files[0]);
  };
}
