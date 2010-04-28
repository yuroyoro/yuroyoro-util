package com.yuroyoro.util.net

object BasicAuth {
  import java.net.{Authenticator,  PasswordAuthentication}

  def apply( username:String, passwd:String ):Unit = {
    Authenticator.setDefault( new Authenticator {
      override def getPasswordAuthentication =
        new PasswordAuthentication(username,  passwd.toCharArray)
    })
  }
}
