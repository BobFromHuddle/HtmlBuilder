namespace Huddle.Foo
open System.Xml.Linq


module HtmlBuilder =

    let xn (s:string) = XName.Get(s)
    type HtmlBuilderState = {
        Doc : XDocument
        El : XElement
        }
    
    let letBind pattern cexpr =
        fun (s:HtmlBuilderState) ->
            let bound = pattern s
            let expr = cexpr(pattern)
            expr (s) 

    
    type Builder () =
            
        member x.Bind(pattern, cexpr) = letBind pattern cexpr
        member x.Zero() = fun (s:HtmlBuilderState) -> ()
        member x.Yield (p) = fun(s:HtmlBuilderState) -> p s |> ignore
        member x.Combine (f1, f2) = x.Bind(f1, fun s -> f2)
        member x.Delay(f) = fun s ->
            let g = f()
            g s

        member __.For(xs, f) = 
            xs |> Seq.fold  (fun acc x -> __.Bind(acc, fun s -> f x)) (__.Zero())

        member x.Head() = ElementBuilder "head"
        member x.Body() = ElementBuilder "body"

    and ElementBuilder (name: string) =
        inherit Builder()

        member x.Run f =
            fun s -> 
                let el = new XElement(xn name)
                s.El.Add(el)
                f( {s with El = el} )  
                

    let text (t:string) = fun (s:HtmlBuilderState) -> 
        s.El.Add( XText(t ) )
        s


    let head = ElementBuilder("head")
    let body = ElementBuilder("body")
    let attr name value state = 
        let a = XAttribute(xn name, value)
        state.El.Add(a)
        state

    let title s = ElementBuilder("title") {
        yield text s
    }

    let para = ElementBuilder("p")

    let scriptSrc src = ElementBuilder("script"){
        yield attr "src" src
        yield attr "type" "text/javascript"
        yield text ""
    }

    let html = Builder()

    let writeTo<'a> (tw:System.IO.TextWriter) (f: HtmlBuilderState -> unit) =
        let doc = XDocument()
        let el = XElement(xn "html")
        doc.Add el
        let s = {Doc = doc; El = el}
        f s
        tw.Write(s.Doc.ToString())

    let sw = new System.IO.StringWriter()   

    let scripts = ["https://code.jquery.com/jquery-2.1.0.min.js"; "https://code.jquery.com/ui/1.9.2/jquery-ui.min.js"]

    let foo = html {
        yield head {
            for s in scripts do
                yield scriptSrc s
            yield title "my document is teh awesomest!"
        }        

        yield body{
            yield para {
                yield text "Hello world!"
            }
        }
    } 
    writeTo sw foo
    let bar = sw.ToString()