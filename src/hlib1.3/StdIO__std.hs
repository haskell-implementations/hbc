module StdIO__std where
import _LibIO_all
import _LibBuiltinFile

stdin, stdout, stderr :: Handle
stdin  = Handle ReadMode _fileStdin
stdout = Handle WriteMode _fileStdout
stderr = Handle WriteMode _fileStderr

