import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm") version "1.7.10"
    antlr
    application
}

group = "org.yeeyu.lau"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    antlr("org.antlr:antlr4:4.11.1")
    testImplementation(kotlin("test"))
}

tasks.test {
    useJUnitPlatform()
}

tasks.withType<KotlinCompile> {
    kotlinOptions.jvmTarget = "18"
}

tasks.compileKotlin {
  dependsOn("generateGrammarSource")
}

tasks.getByName("generateGrammarSource") {
    this as AntlrTask
    arguments = arguments + listOf("-visitor")
}

application {
    mainClass.set("MainKt")
}
