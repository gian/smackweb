structure SmackWeb =
struct
    (** CONFIGURATION **)
    val dataRoot = "/Library/WebServer/CGI-Executables/data/"
    val smackageHome = "/Users/gdpe/.smackage"

    fun esc #"<" = "&lt;"
      | esc #">" = "&gt;"
      | esc #"&" = "&amp;"
      | esc #"\"" = "&quot;"
      | esc c = String.str c

    fun htmlescape s = String.concat (map esc (String.explode s))

    fun searchLink n =
        "<a href=\"index.cgi?action=search&query=" ^ CGI.uriEncode n ^
        "\">" ^ n ^ "</a>"

    fun mkrow (k,v) =
        "<tr><td width=\"35%\" bgcolor=\"#f0f0f0\"><strong>" ^ 
            htmlescape k ^ ":</strong></td>" ^
        "<td width=\"65%\">" ^ v ^ "</td></tr>"

    fun spec2tr (Spec.Provides (s,v)) = 
        mkrow ("Provides", searchLink (htmlescape s) ^ " " ^ SemVer.toString v) 
      | spec2tr (Spec.Description s) = mkrow ("Description", htmlescape s)
      | spec2tr (Spec.Requires (p,v,min)) = mkrow ("Requires", 
          ( searchLink (htmlescape p) ^ " " ^ SemVer.constrToString v ^
          (case min of NONE => "" | SOME v => "(" ^ SemVer.toString v ^ ")")))
      | spec2tr (Spec.Maintainer s) = mkrow ("Maintainer", htmlescape s)
      | spec2tr (Spec.Remote p) = 
            mkrow ("Protocol", htmlescape (Protocol.toString p))
      | spec2tr (Spec.License s) = mkrow ("License", htmlescape s)
      | spec2tr (Spec.Platform s) = mkrow ("Platform", htmlescape s)
      | spec2tr (Spec.Key (k,v)) = mkrow (k, htmlescape v)
     
    fun index () =
        ( CGI.startResponse "text/html"
        ; Template.render ("Home", 
        "<p>Smackage is a simple package manager system for " ^
        "<a href=\"http://www.standardml.org\">Standard ML</a>.</p>" ^
        "<p>At present, most documentation relating to smackage is available " ^
        "from the <a href=\"https://github.com/standardml/smackage\">" ^
        "Smackage GitHub repository</a>.</p>")
        )

    fun mkli n k =
        "<li><a href=\"index.cgi?action=info&package=" ^
        n ^ "&version=" ^ SemVer.toString k ^ "\">" ^
        n ^ " " ^ SemVer.toString k ^ "</a></li>\n"

    fun versionList res =
    if length res = 0 then "No matching packages found." else
            "<h2>Search Results</h2>\n" ^
            "<ul>\n" ^
            String.concatWith "\n"
                (List.map 
                    (fn (n,dict) => "<li>" ^ n ^ "</li>\n" ^
                        "<ul>" ^
                        String.concatWith "\n"
                            (List.map (fn (k,_) => 
                                mkli n k
                             ) (List.rev (SemVerDict.toList dict))) ^
                        "</ul>\n"
                    ) res) ^
            "</ul>\n"

    fun search "" =
        ( CGI.startResponse "text/html"
        ; Template.render ("Search", 
            "<form action=\"index.cgi\" method=\"get\">\n" ^
            "<h3>Query:</h3>\n" ^
            "<input name=\"query\" size=\"35\"/> " ^
            "<input type=\"submit\" value=\"Search\"/>\n" ^
            "<input type=\"hidden\" name=\"action\" value=\"search\"/>\n" ^
            "</form>")
        )
      | search query =
        let
            val _ = VersionIndex.init smackageHome
        in
            ( CGI.startResponse "text/html"
            ; Template.render ("Search", 
                versionList (VersionIndex.search query))
            )
        end

    fun packagelist () =
    let
        val _ = VersionIndex.init smackageHome
    in
        ( CGI.startResponse "text/html"
        ; Template.render ("Package Index",
            versionList (VersionIndex.search ""))
        )
    end

    fun help () =
        ( CGI.startResponse "text/html"
        ; Template.render ("Help", "<p>This is links to smackage docs</p>")
        )

    fun error msg =
        ( CGI.startResponse "text/html"
        ; Template.render ("Error", "<h1>Error: " ^ msg ^ "</h1>\n")
        )

    (* Resolve a package name and version to a safe filesystem path
       relative to dataRoot, preventing obvious attacks like
       ../../../etc/passwd *)
    fun safePath (pkg,ver) =
    let
        (* Do a round trip through semver to validate version *)
        val ver' = SemVer.toString (SemVer.fromString ver)

        val _ =
            if CharVector.all 
                (fn #"-" => true
                  | c => Char.isAlphaNum c) pkg then ()
            else raise Fail "Invalid package name"

        val cand = OS.Path.joinDirFile 
            {dir=dataRoot, file=pkg ^ ".smackspec-v" ^ ver'}
        val cand' = OS.Path.mkAbsolute {path=cand, relativeTo=dataRoot}
        val _ = if not (String.isPrefix dataRoot cand') then
                    raise Fail "Invalid package path"
                else ()
    in
        cand'
    end

    fun info pkg ver =
    let
        val p = safePath (pkg,ver)
        val s = Spec.fromFile p 

        val (name,ver') = Spec.provides s
        val plats = Spec.platforms s
        fun until [] = []
          | until ((Spec.Platform _)::t) = []
          | until (h::t) = h :: until t
        val pre = until s



        val out =
            "<h2>"^ searchLink name ^ " (" ^ SemVer.toString ver' ^ ")</h2>\n" ^
            "<table width=\"85%\" cellpadding=\"16\" cellspacing=\"0\"" ^
            " border=\"1px\">\n" ^
            String.concatWith "\n" (map spec2tr pre) ^
            "</table>\n" ^
            "<h2>Platforms</h2>" ^
            "<ul>\n" ^
            String.concatWith "\n" 
                (map (fn (n,_) => "<li>" ^ n ^ "</li>") plats) ^
            "</ul>\n" ^
            "<h2>Obtaining this package</h2>" ^
            "<p>You can use the following command:</p>\n" ^
            "<blockquote>smackage get " ^ pkg ^ " " ^ ver ^ "</blockquote>\n" 
    in
        ( CGI.startResponse "text/html"
        ; Template.render (pkg, out)
        )
    end handle _ => error ("There was a problem displaying this package")

    fun dispatch () =
        case CGI.getParam "action" of
            "search" => search (CGI.getParam "query")
          | "list" => packagelist ()
          | "" => index ()
          | "home" => index ()
          | "help" => help ()
          | "info" => info (CGI.getParam "package") (CGI.getParam "version") 
          | _ => error ("Page not found") 
    handle _ => error ("There was a problem")
end

val _ = SmackWeb.dispatch ()
