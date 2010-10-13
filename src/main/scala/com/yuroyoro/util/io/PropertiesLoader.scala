package com.yuroyoro.util.io

import java.util.Properties

class PropertiesWrapper( properties:Properties ) {
  def apply(name:String):Option[String] = Option( properties.getProperty(name))
}

trait PropertiesLoader {

  def loadProperties(name:String) = {
    val is = getClass.getResourceAsStream(name)
    val prop = new Properties
    prop.load(is)
    new PropertiesWrapper(prop)
  }
}

