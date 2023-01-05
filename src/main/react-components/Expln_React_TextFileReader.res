@new external createFileReader: unit => {..} = "FileReader"

@react.component
let make = (~onChange:option<(string,string)>=>unit) => {
    <input
        type_="file"
        onChange={evt=>{
            let fr = createFileReader()
            let files = ReactEvent.Synthetic.nativeEvent(evt)["target"]["files"]
            if (files->Js_array2.length == 0) {
                onChange(None)
            } else {
                let file = files[0]
                let fileName = file["name"]
                fr["onload"] = () => {
                    let fileText = fr["result"]
                    onChange(Some(fileName, fileText))
                }
                fr["readAsBinaryString"](. file )
            }
        }}
    />
}