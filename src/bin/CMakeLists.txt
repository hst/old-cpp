add_executable(csp0
  csp0.cc
)

target_link_libraries(csp0 hst)

install(
  TARGETS csp0
  DESTINATION bin
  PERMISSIONS 
    OWNER_READ OWNER_WRITE OWNER_EXECUTE
    GROUP_READ GROUP_EXECUTE
    WORLD_READ WORLD_EXECUTE
)
