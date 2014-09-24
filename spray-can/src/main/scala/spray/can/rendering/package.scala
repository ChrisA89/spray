/*
 * Copyright © 2011-2013 the spray project <http://spray.io>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package spray.can

import spray.http.HttpData
import akka.io.Tcp
import scala.annotation.tailrec
import akka.io.Tcp.{WriteCommand}

package object rendering {
  private[can] def toTcpWriteCommand(data: HttpData, ack: Tcp.Event): Tcp.WriteCommand = {

    /**
     * A tail recursive loop to prevent stack overflow for large messages.
     * @return a list of commands that should be combined to create the WriteCommand.
     */
    @tailrec
    def generateTcpCommands(data: HttpData, ack: Tcp.Event, writeCommands: List[WriteCommand]): List[WriteCommand] = {
      data match {
        case HttpData.Empty                ⇒ Tcp.Write.empty :: writeCommands
        case x: HttpData.SimpleNonEmpty    ⇒ toTcpWriteCommand(x, ack) :: writeCommands
        case HttpData.Compound(head, tail) ⇒ generateTcpCommands(tail, ack, toTcpWriteCommand(head, Tcp.NoAck) :: writeCommands)
      }
    }

    val commands = generateTcpCommands(data, ack, List()).reverse
    commands.tail ++: commands.head
  }

  private[can] def toTcpWriteCommand(data: HttpData.SimpleNonEmpty, ack: Tcp.Event): Tcp.SimpleWriteCommand =
    data match {
      case HttpData.Bytes(byteString)                ⇒ Tcp.Write(byteString, ack)
      case HttpData.FileBytes(fileName, offset, len) ⇒ Tcp.WriteFile(fileName, offset, len, ack)
    }
}
