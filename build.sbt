ivyXML := <dependencies>{xml.XML.load("pom.xml") \\ "dependencies" \\ "dependency" filter (dep => (dep \\ "groupId").text != "org.scala-lang") map (dep => <dependency org={dep \\ "groupId" text} name={dep \\ "artifactId" text} rev={dep \\ "version" text} />)}</dependencies>

compileOrder := CompileOrder.JavaThenScala

