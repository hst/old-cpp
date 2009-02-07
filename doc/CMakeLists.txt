set(DOC_SOURCES
  csp0
  tech-overview
)

set(MAN_SOURCES
  csp0.1
)

set(DOC_DEPENDS)

foreach(_doc ${DOC_SOURCES})
  set(_txt  "${CMAKE_CURRENT_SOURCE_DIR}/${_doc}.txt")
  set(_html "${CMAKE_CURRENT_BINARY_DIR}/${_doc}.html")

  add_custom_command(
    OUTPUT ${_html}
    COMMAND asciidoc -b xhtml11 -o ${_html} ${_txt}
    MAIN_DEPENDENCY ${_txt}
    COMMENT "Generating ${_html}"
  )

  list(APPEND DOC_DEPENDS ${_html})

  install(
    FILES
      ${_html}
    DESTINATION
      share/doc/hst
  )
endforeach(_doc)

foreach(_man ${MAN_SOURCES})
  set(_txt       "${CMAKE_CURRENT_SOURCE_DIR}/${_man}.txt")
  set(_xml       "${CMAKE_CURRENT_BINARY_DIR}/${_man}.xml")
  set(_manoutput "${CMAKE_CURRENT_BINARY_DIR}/${_man}")

  add_custom_command(
    OUTPUT ${_xml}
    COMMAND asciidoc -b docbook -d manpage -o ${_xml} ${_txt}
    MAIN_DEPENDENCY ${_txt}
    COMMENT "Generating ${_xml}"
  )

  add_custom_command(
    OUTPUT ${_manoutput}
    COMMAND
      xmlto
      -o ${CMAKE_CURRENT_BINARY_DIR}
      -m ${CMAKE_CURRENT_SOURCE_DIR}/callouts.xsl
      man ${_xml}
    MAIN_DEPENDENCY ${_xml}
    COMMENT "Generating ${_manoutput}"
  )

  list(APPEND DOC_DEPENDS ${_manoutput})

  string(LENGTH ${_man} _len)
  math(EXPR _len ${_len}-1)
  string(SUBSTRING ${_man} ${_len} 1 _section)

  install(
    FILES
      ${_manoutput}
    DESTINATION
      share/man/man${_section}
  )

endforeach(_man)

add_custom_target(doc DEPENDS ${DOC_DEPENDS})
