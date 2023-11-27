
async function app(github, context, exec) {
    const dateTimeStr = new Date().toLocaleString("nb-NO", { timeZone: "Europe/Oslo" })
    const parts = dateTimeStr.split(".")
    const day =  Number(parts[0])
    const month = Number(parts[1])
    const year = Number(parts[2].substring(0,4))

    if( year == 2023 && month == 12 && day <= 25){
        const targetBrahch = getTargetBranch(day)
        await createNewBranchAndPushItToRemote(exec, targetBrahch)
        await addFile(github, context, `day${day}/input.txt`, "input", targetBrahch)
        await addFile(github, context, `day${day}/test-input.txt`, "test-input", targetBrahch)
        await addFile(github, context, `day${day}/day${day}.fsx`, fsxFileContent, targetBrahch)
    }   
}

const fsxFileContent = `
open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let task1 = "task 1"
let task2 = "task 2"

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
`


const getTargetBranch = (day) => `day-${day}`

async function createNewBranchAndPushItToRemote(exec, targetBranch) {
    try {
        await exec.exec("git", ["checkout", "-b", targetBranch])
        await exec.exec("git", ["push", "origin", targetBranch, "-f"])
    } catch (error) {
        console.log(error)
    }
}

async function addFile(github, context, filePath, fileContent, targetBranch) {
    await github.rest.repos.createOrUpdateFileContents({
        owner: context.repo.owner,
        repo: context.repo.repo,
        path: filePath,
        message: `Added file ${filePath}`,
        content: Buffer.from(fileContent).toString('base64'),
        branch: targetBranch
    })
}

module.exports = async (github, context, exec) => {
    return app(github, context, exec)
}