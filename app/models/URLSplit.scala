package models

trait URLSplit[A] {
  def from(uri: String): A
  def to(a: A): String
}
