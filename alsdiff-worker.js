importScripts('https://cdnjs.cloudflare.com/ajax/libs/pako/2.1.0/pako.min.js');
importScripts('/alsdiff.js');

self.__alsdiff_files = {};
let fileCounter = 0;

self.onmessage = async function(e) {
  const msg = e.data;

  if (msg.type === 'diff') {
    const { requestId, file1, file2, options } = msg;
    const id1 = fileCounter++;
    const id2 = fileCounter++;
    self.__alsdiff_files[id1] = file1;
    self.__alsdiff_files[id2] = file2;

    try {
      const result = await self.alsdiff.diffFilesById(
        id1, id2, file1.name, file2.name, options
      );
      self.postMessage({ type: 'result', requestId, result });
    } catch (error) {
      self.postMessage({
        type: 'error', requestId,
        error: (error && error.message) ? error.message : String(error)
      });
    } finally {
      delete self.__alsdiff_files[id1];
      delete self.__alsdiff_files[id2];
    }
  } else if (msg.type === 'setDebug') {
    if (self.alsdiff && typeof self.alsdiff.setDebug === 'function') {
      self.alsdiff.setDebug(msg.enabled);
    }
  }
};
