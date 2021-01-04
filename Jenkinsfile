pipeline {
    agent any
    options {
        buildDiscarder(logRotator(numToKeepStr: '3'))
    }
    environment {
        IMAGE = 'glpgintextsummarytable'
        NS = 'oa'
        REG = '196229073436.dkr.ecr.eu-west-1.amazonaws.com'
        TAG = sh(returnStdout: true, script: "echo $BRANCH_NAME | sed 's/[^a-z0-9._-]/./g'").trim()
    }
    stages {
        stage('Build Image') {
            agent {
                kubernetes {
                    yaml '''
                    apiVersion: v1
                    kind: Pod
                    spec:
                      containers:
                      - name: dind
                        image: 196229073436.dkr.ecr.eu-west-1.amazonaws.com/oa-infrastructure/dind
                        securityContext:
                          privileged: true'''
                    defaultContainer 'dind'
                }
            }
            steps {
                ecrPull "${env.REG}", "${env.NS}/${env.IMAGE}", "${env.TAG}", '', 'eu-west-1'
                copyArtifacts filter: '*.tar.gz', fingerprintArtifacts: true, projectName: 'git/glpgStyle/master', selector: lastSuccessful()
                copyArtifacts filter: '*.tar.gz', fingerprintArtifacts: true, projectName: 'git/GLPGUtilityFct/master', selector: lastSuccessful()
                sh "docker build --cache-from ${env.REG}/${env.NS}/${env.IMAGE}:${env.TAG} -t ${env.NS}/${env.IMAGE}:${env.TAG} -f Dockerfile.build ."
                ecrPush "${env.REG}", "${env.NS}/${env.IMAGE}", "${env.TAG}", '', 'eu-west-1'
            }
        }
        stage('Packages') {
            agent {
                kubernetes {
                    yaml """
                    apiVersion: v1
                    kind: Pod
                    spec:
                      containers:
                      - name: r
                        image: ${env.REG}/${env.NS}/${env.IMAGE}:${env.TAG}
                        command: 
                        - cat
                        tty: true
                        imagePullPolicy: Always"""
                    defaultContainer 'r'
                }
            }
            stages {
                stage('inTextSummaryTable') {
                    stages {
                        stage('Roxygen') {
                            steps {
                                sh 'R -q -e \'roxygen2::roxygenize("package/inTextSummaryTable", load = "source")\''
                            }
                        }
                        stage('Build') {
                            steps {
                                sh 'R CMD build package/inTextSummaryTable'
                            }
                        }
                        stage('Check') {
                            steps {
                                sh '''
                                export TESTTHAT_DEFAULT_CHECK_REPORTER="junit"
                                export TESTTHAT_OUTPUT_FILE="results.xml"
                                ls inTextSummaryTable_*.tar.gz && R CMD check inTextSummaryTable_*.tar.gz --no-manual
                                '''
                            }
                            post {
                                always {
                                    junit "*.Rcheck/tests/results.xml"
                                }
                            }
                        }
                        stage('Coverage') {
                           steps {
                                sh '''
                                R -q -e \'
                                pc <- covr::package_coverage("package/inTextSummaryTable");
                                covr::report(x = pc, file = paste0("testCoverage-inTextSummaryTable", packageVersion("inTextSummaryTable"), ".html"))
                                covr::to_cobertura(pc)
                                \'
                                '''
                            }
                           post {
                              success {
                                  cobertura autoUpdateHealth: false, autoUpdateStability: false, coberturaReportFile: 'cobertura.xml', conditionalCoverageTargets: '70, 0, 0', failUnhealthy: false, failUnstable: false, lineCoverageTargets: '80, 0, 0', maxNumberOfBuilds: 0, methodCoverageTargets: '80, 0, 0', onlyStable: false, sourceEncoding: 'ASCII', zoomCoverageChart: false
                              }
                           }
                        }
                        stage('Install') {
                            steps {
                                sh 'R -q -e \'install.packages(list.files(".", "inTextSummaryTable_.*.tar.gz"), repos = NULL) \''
                            }
                        }
                    }
                }
                stage('Archive artifacts') {
                    steps {
                        archiveArtifacts artifacts: '*.tar.gz, *.pdf, **/00check.log, **/testthat.Rout, **/testCoverage*.html', fingerprint: true
                    }
                }
            }
        }
    }
}

