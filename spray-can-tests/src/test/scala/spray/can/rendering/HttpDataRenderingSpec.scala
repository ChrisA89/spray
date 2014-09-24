package spray.can.rendering

import org.specs2.mutable.Specification
import akka.util.ByteString
import spray.http.HttpData
import akka.io.Tcp
import java.io.{ FileOutputStream, File }

class HttpDataRenderingSpec extends Specification {
  val bytes = ByteString(Array.fill(10)('a'.toByte))
  val file = {
    val res = File.createTempFile("test", "dat").getCanonicalFile
    val os = new FileOutputStream(res)
    os.write(Array.fill(2000)('b'.toByte))
    os.close()
    res.deleteOnExit()
    res
  }

  "HttpData" should {
    "render correctly" in {
      "Bytes" in {
        toTcpWriteCommand(HttpData(bytes), CustomAck) === Tcp.Write(bytes, CustomAck)
      }
      "FileBytes" in {
        toTcpWriteCommand(HttpData(file, 1000, 500), CustomAck) === Tcp.WriteFile(file.getCanonicalPath, 1000, 500, CustomAck)
      }
      "Compound" in {
        toTcpWriteCommand(HttpData(bytes) +: HttpData(file, 1000, 500), CustomAck) ===
          (Tcp.Write(bytes) +:
            Tcp.WriteFile(file.getCanonicalPath, 1000, 500, CustomAck))
      }
      "Lots of data" in {
        val lotsOfData = (0 to 4000).foldLeft(HttpData(bytes)) {
          (all,_) => HttpData(bytes) +: all
        }
        toTcpWriteCommand(lotsOfData, CustomAck) must not(throwA[StackOverflowError])
        // Calling equals on the result will throw a StackOverflowError.
      }
    }
  }

  object CustomAck extends Tcp.Event
}
