package utils.backup

import java.io.File

import base.TestBaseDefinition
import org.scalatest.WordSpec

class FolderZipperSpec extends WordSpec with TestBaseDefinition {

  val target = "test/resources/zip/"
  val subject = "test/resources/origins"

  val folderZipper = new FolderZipper

  /* tupel : (path: String, checksum: String) */
  val tupel = folderZipper.zipFolder(subject, target)

  val file = new File(tupel._1)


  "A FolderZipper" should {

    "prove that a given file exists " in {
      file should be a 'file
      file should exist
    }

    "prove that a given file is readable " in {
      file should be(readable)
    }

    "returned the corresponding checksum for given file" in {

      val expectedChecksum = folderZipper.checkSum(tupel._1)

      expectedChecksum shouldEqual tupel._2

    }
  }


}
