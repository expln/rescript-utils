import Release.Action.INCREASE_VERSION
import Release.Action.PREPARE_RELEASE
import java.io.BufferedReader
import java.io.File
import java.io.InputStreamReader
import java.util.regex.Matcher
import java.util.regex.Pattern
import java.util.regex.Pattern.compile

enum class Action {
    PREPARE_RELEASE, INCREASE_VERSION
}

val action = Action.valueOf(args[0])

when (action) {
    PREPARE_RELEASE -> Tools.prepareRelease()
    INCREASE_VERSION -> Tools.increaseVersion()
}

object Tools {
    private val versionPattern = ".*\"version\":\\s*\"(\\d+)\\.(\\d+)\\.(\\d+)\",.*"

    fun prepareRelease() {
        checkWorkingDirectory()

        clean()
        buildProject()

        val releaseVersion = getCurrVersionName()
        val tagName = "v-$releaseVersion"
        tag(tagName)
//        log("<<<tag>>>: " + tagName)

        pack()

        log("""
            Next commands:
            --------------------------------
            npm login
            npm publish --access public
            [increase version]
            --------------------------------
        """.trimIndent())

        println("Done.")
    }

    fun increaseVersion() {
        val releaseVersion = getCurrVersionName()
        incProjectVersion()
        val newDevVersion = getCurrVersionName()

        commit("Increase version from ${releaseVersion} to ${newDevVersion}")
//        log("<<<commit>>>: Increase version from ${releaseVersion} to ${newDevVersion}")

        println("Done.")
    }

    private fun checkWorkingDirectory() {
        log("checkFiles")
        runCommand(
            "git status",
            compile("nothing to commit, working tree clean")
        ) ?: throw RuntimeException("Working directory is not clean.")
    }

    private fun clean() {
        log("clean")
        val result: Pair<String?, Matcher?>? = runCommand(
            "npm run clean",
            compile("^Cleaning completed\\.$")
        )
        if (result == null || result.first?.contains("BUILD FAILED")?:true) {
            throw RuntimeException("Project cleaning failed.")
        }
    }

    private fun buildProject() {
        log("buildProject")
        val result: Pair<String?, Matcher?>? = runCommand(
            "npm run compile-with-deps-and-test",
            compile(".*\\d+ passing \\(\\d+ms\\).*")
        )
        if (result == null || result.first?.contains("BUILD FAILED")?:true) {
            throw RuntimeException("Project build failed.")
        }
    }

    private fun pack() {
        log("pack")
        val result: Pair<String?, Matcher?>? = runCommand(
            "npm pack && echo Pack completed.",
            compile("^Pack completed.$")
        )
        if (result == null || result.first?.contains("BUILD FAILED")?:true) {
            throw RuntimeException("Project pack failed.")
        }
    }

    private fun commit(commitMessage: String) {
        val exitCode = runCommandForExitValue("git commit -a -m \"$commitMessage\"")
        if (0 != exitCode) {
            throw RuntimeException("exitCode = $exitCode")
        }
    }

    private fun tag(tagName: String) {
        val exitCode = runCommandForExitValue("git tag $tagName")
        if (0 != exitCode) {
            throw RuntimeException("exitCode = $exitCode")
        }
    }

    private fun getCurrVersionName(): String {
        val matcher = compile(versionPattern, Pattern.DOTALL).matcher(File("./package.json").readText())
        if (!matcher.matches()) {
            throw RuntimeException("Cannot extract curent version.")
        } else {
            return "${matcher.group(1)}.${matcher.group(2)}.${matcher.group(3)}"
        }
    }

    private fun incProjectVersion(): String {
        log("incProjectVersion")
        val packageJsonFile = File("./package.json")
        var newVersion: String? = null
        replace(
            srcFile = packageJsonFile,
            pattern = compile(versionPattern),
            dstFile = packageJsonFile
        ) { matcher ->
            newVersion = "${matcher.group(1)}.${matcher.group(2)}.${matcher.group(3).toLong()+1}"
            "  \"version\": \"${newVersion}\","
        }
        if (newVersion == null) {
            throw RuntimeException("Failed to increase project version in package.json.")
        }
        val bsConfigJsonFile = File("./bsconfig.json")
        var numOfMatches = 0
        replace(
            srcFile = bsConfigJsonFile,
            pattern = compile(versionPattern),
            dstFile = bsConfigJsonFile
        ) {
            numOfMatches++
            "  \"version\": \"${newVersion}\","
        }
        if (numOfMatches != 1) {
            throw RuntimeException("Failed to increase project version in bsconfig.json.")
        }
        return newVersion!!
    }

    private fun replace(srcFile: File, pattern: Pattern, dstFile: File, replacement: (Matcher) -> String?) {
        val newContent: String = replace(srcFile.readText(), pattern, replacement)
        dstFile.parentFile.mkdirs()
        dstFile.writeText(newContent)
    }

    private fun replaceSubstringInFile(file: File, oldValue: String, newValue: String) {
        file.writeText(file.readText().replace(oldValue, newValue))
    }

    private fun runCommand(command: String, pattern: Pattern): Pair<String, Matcher>? {
        return startProcess(command) { process, processOutput ->
            val result = readTill(processOutput, pattern)
            process.destroy()
            result
        }
    }

    private fun runCommandForExitValue(command: String): Int {
        return startProcess(command) { process, processOutput ->
            readTill(processOutput, null)
            process.waitFor()
        }
    }

    private fun <T> startProcess(command: String, outputConsumer: (Process, BufferedReader) -> T): T {
        log("Command: $command")
        val builder = ProcessBuilder("cmd.exe", "/c", command)
        builder.redirectErrorStream(true)
        val proc: Process = builder.start()
        BufferedReader(InputStreamReader(proc.inputStream)).use { reader ->
            return outputConsumer(proc, reader)
        }
    }

    private fun readTill(reader: BufferedReader, pattern: Pattern?): Pair<String, Matcher>? {
        var matcher: Matcher? = null
        var line: String?
        do {
            line = reader.readLine()
            log(line)
            if (line == null) {
                return null
            }
            if (pattern != null) {
                matcher = pattern.matcher(line)
            }
        } while (line != null && (matcher == null || !matcher.matches()))
        return Pair(line!!, matcher!!)
    }

    private fun log(msg: String) {
        println("release>>> $msg")
    }

    fun replace(content: String, pattern: Pattern, replacement: (Matcher) -> String?): String {
        val matcher = pattern.matcher(content)
        val newContent = StringBuilder()
        var prevEnd = 0
        while (matcher.find()) {
            newContent.append(content, prevEnd, matcher.start())
            val replacementValue = replacement(matcher)
            if (replacementValue != null) {
                newContent.append(replacementValue)
            } else {
                newContent.append(matcher.group(0))
            }
            prevEnd = matcher.end()
        }
        newContent.append(content, prevEnd, content.length)
        return newContent.toString()
    }

}