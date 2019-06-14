#!/bin/sh
set -e
if test -d "$PWD"; then
  docker run -it -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:next "$@"
else
  echo "Cannot detect the current directory."
  echo "This often happens when the current directory gets renamed or moved."
  echo "Trying to cd to the desired folder may solve this."
  exit 1
fi
# Do not remove the next line. It is used as an approximate witness that the download of this file was complete. This string should not appear anywhere else in the file.
# END OF DOWNLOADED FILE
