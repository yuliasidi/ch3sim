########################################################################
s <- ssh::ssh_connect( Sys.getenv('UCONN_USER_HPC'))
rfile.sub0 <- 'mcda_c4_sc2_pmiss90_cartTRUE_mar_5l.R'

tmpl <- whisker::whisker.render(template = readLines('tmpls/ch3.slurm.tmpl'), 
                                data = list(rfile = rfile.sub0,
                                            n=20,
                                            time = 60))
cat(tmpl, '\n', file ="ch3.slurm")
ssh::scp_upload(s,c(sprintf('pgms_sim/%s', rfile.sub0),'ch3.slurm'))
condor::ssh_fn(s,'sbatch ch3.slurm')

########################################################################
s <- ssh::ssh_connect( Sys.getenv('UCONN_USER_HPC'))
rfile.sub0 <- 'mcda_c4_sc2_pmiss90_normFALSE_mar2.R'

tmpl <- whisker::whisker.render(template = readLines('tmpls/ch3.slurm.tmpl'), 
                                data = list(rfile = rfile.sub0,
                                            n=24,
                                            time = 60))
cat(tmpl, '\n', file ="ch3.slurm1")
ssh::scp_upload(s,c(sprintf('pgms_sim/%s', rfile.sub0),'ch3.slurm1'))
condor::ssh_fn(s,'sbatch ch3.slurm1')

########################################################################
s <- ssh::ssh_connect( Sys.getenv('UCONN_USER_HPC'))
rfile.sub0 <- 'mcda_c4_sc2_pmiss90_normTRUE_mar2.R'

tmpl <- whisker::whisker.render(template = readLines('tmpls/ch3.slurm.tmpl'), 
                                data = list(rfile = rfile.sub0,
                                            n=24,
                                            time = 60))
cat(tmpl, '\n', file ="ch3.slurm2")
ssh::scp_upload(s,c(sprintf('pgms_sim/%s', rfile.sub0),'ch3.slurm2'))
condor::ssh_fn(s,'sbatch ch3.slurm2')


########################################################################
s <- ssh::ssh_connect( Sys.getenv('UCONN_USER_HPC'))
rfile.sub0 <- 'mcda_c4_sc2_pmiss90_cartTRUE_mar.R'

tmpl <- whisker::whisker.render(template = readLines('tmpls/ch3.slurm.tmpl'), 
                                data = list(rfile = rfile.sub0,
                                            n=24,
                                            time = 60))
cat(tmpl, '\n', file ="ch3.slurm3")
ssh::scp_upload(s,c(sprintf('pgms_sim/%s', rfile.sub0),'ch3.slurm3'))
condor::ssh_fn(s,'sbatch ch3.slurm3')

########################################################################
s <- ssh::ssh_connect( Sys.getenv('UCONN_USER_HPC'))
rfile.sub0 <- 'mcda_c4_sc2_pmiss80_normTRUE_mar.R'

tmpl <- whisker::whisker.render(template = readLines('tmpls/ch3.slurm.tmpl'), 
                                data = list(rfile = rfile.sub0,
                                            n=20,
                                            time = 60))
cat(tmpl, '\n', file ="ch3.slurm4")
ssh::scp_upload(s,c(sprintf('pgms_sim/%s', rfile.sub0),'ch3.slurm4'))
condor::ssh_fn(s,'sbatch ch3.slurm4')

########################################################################
s <- ssh::ssh_connect( Sys.getenv('UCONN_USER_HPC'))
rfile.sub0 <- 'mcda_c4_sc2_pmiss80_cartTRUE_mar.R'

tmpl <- whisker::whisker.render(template = readLines('tmpls/ch3.slurm.tmpl'), 
                                data = list(rfile = rfile.sub0,
                                            n=24,
                                            time = 40))
cat(tmpl, '\n', file ="ch3.slurm5")
ssh::scp_upload(s,c(sprintf('pgms_sim/%s', rfile.sub0),'ch3.slurm5'))
condor::ssh_fn(s,'sbatch ch3.slurm5')

########################################################################
s <- ssh::ssh_connect( Sys.getenv('UCONN_USER_HPC'))
rfile.sub0 <- 'mcda_c4_sc2_pmiss70_normTRUE_mar.R'

tmpl <- whisker::whisker.render(template = readLines('tmpls/ch3.slurm.tmpl'), 
                                data = list(rfile = rfile.sub0,
                                            n=24,
                                            time = 40))
cat(tmpl, '\n', file ="ch3.slurm6")
ssh::scp_upload(s,c(sprintf('pgms_sim/%s', rfile.sub0),'ch3.slurm6'))
condor::ssh_fn(s,'sbatch ch3.slurm6')

########################################################################
s <- ssh::ssh_connect( Sys.getenv('UCONN_USER_HPC'))
rfile.sub0 <- 'mcda_c4_sc2_pmiss70_cartTRUE_mar.R'

tmpl <- whisker::whisker.render(template = readLines('tmpls/ch3.slurm.tmpl'), 
                                data = list(rfile = rfile.sub0,
                                            n=24,
                                            time = 40))
cat(tmpl, '\n', file ="ch3.slurm7")
ssh::scp_upload(s,c(sprintf('pgms_sim/%s', rfile.sub0),'ch3.slurm7'))
condor::ssh_fn(s,'sbatch ch3.slurm7')



