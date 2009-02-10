set(DOC_SOURCES
  csp0
  tech-overview
)

set(MAN_SOURCES
  csp0.1
)

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/hst.css
  ${CMAKE_CURRENT_BINARY_DIR}/hst.css
  COPYONLY
)

install(
  FILES
    ${CMAKE_CURRENT_BINARY_DIR}/hst.css
  DESTINATION
    share/doc/hst
)

set(DOC_DEPENDS)

foreach(_doc ${DOC_SOURCES})
  set(_md   "${CMAKE_CURRENT_SOURCE_DIR}/${_doc}.md")
  set(_html "${CMAKE_CURRENT_BINARY_DIR}/${_doc}.html")

  add_custom_command(
    OUTPUT ${_html}
    COMMAND pandoc -c hst.css -s -o ${_html} ${_md}
    MAIN_DEPENDENCY ${_md}
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
  set(_md        "${CMAKE_CURRENT_SOURCE_DIR}/${_man}.md")
  set(_html      "${CMAKE_CURRENT_BINARY_DIR}/${_man}.html")
  set(_manoutput "${CMAKE_CURRENT_BINARY_DIR}/${_man}")

  add_custom_command(
    OUTPUT ${_html}
    COMMAND pandoc -c hst.css -s -o ${_html} ${_md}
    MAIN_DEPENDENCY ${_md}
    COMMENT "Generating ${_html}"
  )

  add_custom_command(
    OUTPUT ${_manoutput}
    COMMAND pandoc -s -t man -o ${_manoutput} ${_md}
    MAIN_DEPENDENCY ${_md}
    COMMENT "Generating ${_manoutput}"
  )

  list(APPEND DOC_DEPENDS ${_html} ${_manoutput})

  string(LENGTH ${_man} _len)
  math(EXPR _len ${_len}-1)
  string(SUBSTRING ${_man} ${_len} 1 _section)

  install(
    FILES
      ${_manoutput}
    DESTINATION
      share/man/man${_section}
  )

  install(
    FILES
      ${_html}
    DESTINATION
      share/doc/hst
  )

endforeach(_man)

add_custom_target(doc DEPENDS ${DOC_DEPENDS})
