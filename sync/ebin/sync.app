{application,sync,
             [{description,"Sync - Automatic Code Reloader"},
              {applications,[kernel,stdlib,syntax_tools,compiler]},
              {vsn,"0.2.0"},
              {registered,[]},
              {mod,{sync,[]}},
              {maintainers,["Rusty Klophaus","Jesse Gumm","Heinz N. Gies"]},
              {licenses,["MIT"]},
              {links,[{"Github","https://github.com/rustyio/sync"}]},
              {applications,[kernel,stdlib,fs]},
              {env,[{discover_modules_interval,10000},
                    {discover_src_dirs_interval,10000},
                    {discover_src_files_interval,5000},
                    {compare_beams_interval,2000},
                    {compare_src_files_interval,1000},
                    {file_variables,"-*- mode: compilation; mode: auto-revert; buffer-read-only: true; auto-revert-interval: 0.5 -*-\n\n"},
                    {out_file,"/tmp/sync.out"},
                    {excluded_modules,[]}]},
              {modules,[sync,sync_notify,sync_options,sync_scanner,
                        sync_utils]}]}.