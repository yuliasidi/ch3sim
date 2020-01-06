########################################################################
s <- ssh::ssh_connect( Sys.getenv('UCONN_USER_HPC'))
rfile.sub0 <- 'mcda_c3_sc1_pmiss90.R'

tmpl <- whisker::whisker.render(template = readLines('tmpls/ch3.slurm.tmpl'), 
                                data = list(rfile = rfile.sub0,
                                            n=24,
                                            time = 30))
cat(tmpl, '\n', file ="ch3.slurm")
ssh::scp_upload(s,c(sprintf('pgms_sim/%s', rfile.sub0),'ch3.slurm'))
condor::ssh_fn(s,'sbatch ch3.slurm')

########################################################################
s <- ssh::ssh_connect( Sys.getenv('UCONN_USER_HPC'))
rfile.sub0 <- 'mcda_c2_sc1_pmiss90_sdlower.R'

tmpl <- whisker::whisker.render(template = readLines('tmpls/ch3.slurm.tmpl'), 
                                data = list(rfile = rfile.sub0,
                                            n=24,
                                            time = 30))
cat(tmpl, '\n', file ="ch3.slurm1")
ssh::scp_upload(s,c(sprintf('pgms_sim/%s', rfile.sub0),'ch3.slurm1'))
condor::ssh_fn(s,'sbatch ch3.slurm1')

########################################################################
s <- ssh::ssh_connect( Sys.getenv('UCONN_USER_HPC'))
rfile.sub0 <- 'mcda_c2_sc1_pmiss60.R'

tmpl <- whisker::whisker.render(template = readLines('tmpls/ch3.slurm.tmpl'), 
                                data = list(rfile = rfile.sub0,
                                            n=24,
                                            time = 30))
cat(tmpl, '\n', file ="ch3.slurm2")
ssh::scp_upload(s,c(sprintf('pgms_sim/%s', rfile.sub0),'ch3.slurm2'))
condor::ssh_fn(s,'sbatch ch3.slurm2')


########################################################################
s <- ssh::ssh_connect( Sys.getenv('UCONN_USER_HPC'))
rfile.sub0 <- 'mcda_c2_sc1_pmiss50.R'

tmpl <- whisker::whisker.render(template = readLines('tmpls/ch3.slurm.tmpl'), 
                                data = list(rfile = rfile.sub0,
                                            n=24,
                                            time = 30))
cat(tmpl, '\n', file ="ch3.slurm3")
ssh::scp_upload(s,c(sprintf('pgms_sim/%s', rfile.sub0),'ch3.slurm3'))
condor::ssh_fn(s,'sbatch ch3.slurm3')
