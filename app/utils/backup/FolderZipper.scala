package utils.backup

import java.io.{File, FileInputStream, FileOutputStream}
import java.util.zip.{ZipEntry, ZipOutputStream}
import org.joda.time.DateTime


class FolderZipper {

  def zipFolder(src: String, dest: String): (String, String) = {

    val theDir: File = new File(dest);
    if (!theDir.exists()) theDir.mkdir();

    val destFile: String = dest + timestamp() + ".zip"

    val fileOut: FileOutputStream = new FileOutputStream(destFile)
    val zip: ZipOutputStream = new ZipOutputStream(fileOut)

    addFolderToZip("", src, zip)
    zip.flush()
    zip.close()
    val checksum: String = checkSum(destFile)

    (destFile, checksum)


  }

  def addFileToZip(path: String, src: String, dest: ZipOutputStream): Unit = {

    val folder: File = new File(src)

    if (folder.isDirectory) {
      addFolderToZip(path, src, dest)
    } else {
      val byteBuffer = Array.ofDim[Byte](1024)
      val in: FileInputStream = new FileInputStream(src)
      dest.putNextEntry(new ZipEntry(path + "/" + folder.getName))
      var length: Int = in.read(byteBuffer)
      while (length > 0) {
        dest.write(byteBuffer, 0, length)
        length = in.read(byteBuffer)
      }
    }

  }

  def addFolderToZip(path: String, src: String, dest: ZipOutputStream): Unit = {

    val folder: File = new File(src)

    for (fileName <- folder.list()) {
      if (path.equals("")) {
        addFileToZip(folder.getName(), src + "/" + fileName, dest)
      } else {
        addFileToZip(path + "/" + folder.getName(), src + "/" + fileName, dest)
      }
    }

  }

  def timestamp(): String = {
    val now: DateTime = DateTime.now
    now.toString("yyyyMMdd_HHmmss")
  }

  def checkSum(file: String): String = {

    val checkFile: File = new File(file)
    val fis: FileInputStream = new FileInputStream(checkFile);
    val md5: String = org.apache.commons.codec.digest.DigestUtils.md5Hex(fis);
    fis.close()
    md5
  }

}