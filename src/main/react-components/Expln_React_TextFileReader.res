@new external createFileReader: unit => {..} = "FileReader"

@react.component
let make = (~onChange: option<string=>'a>=?) => {
   <input type_="file" onChange={evt=>{
        switch onChange {
            | Some(clbk) => {
                let fr = createFileReader()
                fr["onload"] = () => {
                    fr["result"]->clbk
                }
                fr["readAsBinaryString"](. ReactEvent.Synthetic.nativeEvent(evt)["target"]["files"][0])
            }
            | _ => ()
        }
   }}  /> 
}