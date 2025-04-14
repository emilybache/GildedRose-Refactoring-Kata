plugins {
	kotlin("jvm") version "2.1.20"
	application
}

group = "com.gildedrose"
version = "1.0-SNAPSHOT"

repositories {
	mavenCentral()
}

java {
	toolchain {
		languageVersion = JavaLanguageVersion.of(8)
	}
}

dependencies {
	implementation(kotlin("stdlib"))
	testImplementation(kotlin("test"))
	testImplementation("org.junit.jupiter:junit-jupiter:5.12.2")
}

tasks.test {
	useJUnitPlatform()
}

tasks.register<JavaExec>("texttest") {
	description = "Allow you to run text-based approval tests with texttest"
	group = JavaBasePlugin.BUILD_TASK_NAME
	mainClass.set("com.gildedrose.TexttestFixtureKt")
	classpath = sourceSets["test"].runtimeClasspath
	args("30")
}


application {
	mainClass.set("com.gildedrose.TexttestFixtureKt")
}
