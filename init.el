(setq-default
 ad-redefinition-action 'accept         ; Silence warnings for redefinition
 custom-unlispify-menu-entries nil      ; Prefer kebab-case for titles
 custom-unlispify-tag-names nil         ; Prefer kebab-case for symbols
(put 'downcase-region 'disabled nil)    ; Enable downcase-region
(put 'upcase-region 'disabled nil)      ; Enable upcase-region
 native-comp-async-report-warnings-errors 'silent ; Skip compilation error buffers
 read-process-output-max (* 1024 1024)  ; Increase read size per process
