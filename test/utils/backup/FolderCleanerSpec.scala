package utils.backup

import java.io.{File, FileOutputStream}

import base.TestBaseDefinition
import org.joda.time.DateTime
import org.scalatest._


class FolderCleanerSpec extends WordSpec with TestBaseDefinition with BeforeAndAfterAll {

  val target = "test/resources/zip/"

  val folderCleaner = new FolderCleaner

  val count = 10

  init()

  override def afterAll() =  {
    folderCleaner.clean(target, 0)
  }

  "A FolderCleaner" should {

    "delete Files which are older than given amount of days" in {

      var days = 0
      var rest = 0
      if (count % 2 == 0) {
        days = count / 2 + 1
        rest = days - 1
      } else {
        days = (count + 1) / 2
        rest = days
      }

      val countBefore = countFiles()

      // method to test
      folderCleaner.clean(target, days)

      val countAfter = countFiles()

      (countBefore - countAfter) shouldEqual (rest)

    }


  }

  def init(): Unit = {

    var dateArray = new Array[String](count)

    var i = 0
    for (i <- 1 to count) {
      dateArray(i - 1) = folderCleaner.stringFromDate(DateTime.now.minusDays(i))
    }

    val theDir: File = new File("src/test/resources/zip")
    if (!theDir.exists()) theDir.mkdir();

    for (date <- dateArray) {
      createFile(date)
    }
  }

  def createFile(file: String): Unit = {

    val path: String = target + file + ".zip"

    val yourFile: File = new File(path)
    if (!yourFile.exists()) {
      yourFile.createNewFile();
    }
    val oFile: FileOutputStream = new FileOutputStream(yourFile, false);

  }

  def countFiles(): Int = {

    var fileCounter = 0
    val theDir = new File(target)

    theDir.list().length

  }

  def cleanAfterTesting(): Unit = {
    folderCleaner.clean(target, 0)
  }

}
