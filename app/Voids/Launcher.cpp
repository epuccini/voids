/*************************************** Includes *********************************/

#include <QPainter>
#include <QFileDialog>
#include <QEvent>
#include <QDialog>
#include <QDebug>
#include <QTreeWidget>
#include <QProcess>
#include <QString>
#include <QMessageBox>
#include <QThread>
#include <QWindow>

#include "Launcher.h"
#include "mainwindow.h"
#include "ui_mainwindow.h"

/*************************************** Const *************************************/

extern QList<Launcher*> qlLauncherList;

#ifdef Q_OS_WIN
const QSize IMAGE_WINDOW_SIZE_MAX(820,820);
#endif

#ifdef Q_OS_UNIX
const QSize IMAGE_WINDOW_SIZE_MAX(820,820);
#endif

/*************************************** Implementation ****************************/

Launcher::Launcher(QWidget *parent, QMenu* menu)
	: LauncherWidget(parent)
{
    formLauncher.setupUi(this);
    menuWindows = menu;
    mainWindow = parent;
	isInitialized = false;
    //setAttribute(Qt::WA_NativeWindow, true);
    validationScene = nullptr;
    errorScene = nullptr;
    validationImageView = nullptr;
    errorImageView = nullptr;
#ifdef ECL_BUILD
    lispEngine = nullptr;
#else
    simulationProcess = nullptr;
    setupStyleSheets();
#endif
    validationScene = nullptr;
    errorScene = nullptr;
    validationImageViewProcess = nullptr;
    errorImageViewProcess = nullptr;
    lispSettings = nullptr;
    formLauncher.progressBar->reset();
#ifdef Q_OS_WIN
    formLauncher.progressBar->setMinimumWidth(360);
    formLauncher.progressBar->setMaximumWidth(360);
#endif
}


Launcher::~Launcher()
{
    if (validationScene)
        delete validationScene;
    if (errorScene)
        delete errorScene;
    if (validationImageViewProcess)
        delete validationImageViewProcess;
    if (errorImageViewProcess)
        delete errorImageViewProcess;
    if (lispSettings)
        delete lispSettings;
#ifdef ECL_BUILD
    if(lispEngine)
        delete lispEngine;
#endif
}

Ui::FormLauncher& Launcher::getLauncher(void)
{
    return formLauncher;
}

void Launcher::showEvent(QShowEvent *event)
{
	LauncherWidget::showEvent(event);
}

void Launcher::setupStyleSheets(void)
{
    formLauncher.widgetLauncher->setStyleSheet("background-color: #222222");
    formLauncher.gridLayoutWidget->setStyleSheet("background-color: #222222");

    formLauncher.lineEditConfigurationFile->setStyleSheet(("background-color: #FFFFFF; color: #000000"));

    formLauncher.groupBoxOptions->setStyleSheet(("background-color: #333333; color: #EEEEEE"));
    formLauncher.groupBoxOutput->setStyleSheet("background-color: #333333; color: #EEEEEE");
    formLauncher.groupBoxData->setStyleSheet("background-color: #333333; color: #EEEEEE");

    formLauncher.graphicsViewValidation->setStyleSheet("background-color: #FFFFFF");
    formLauncher.graphicsViewError->setStyleSheet(("background-color: #FFFFFF"));

    formLauncher.textEditConsole->setStyleSheet("background-color: #FFFFFF; color: black");

    formLauncher.pushButtonConfiguration->setMaximumHeight(20);
    formLauncher.pushButtonStart->setMaximumHeight(20);
    formLauncher.pushButtonStop->setMaximumHeight(20);
    formLauncher.pushButtonValidation->setMaximumHeight(20);
    formLauncher.pushButtonError->setMaximumHeight(20);
    formLauncher.pushButtonConfiguration->setStyleSheet("background-color: #555555; color: #EEEEEE");
    formLauncher.pushButtonStart->setStyleSheet("background-color: #555555; color: #EEEEEE");
    formLauncher.pushButtonStop->setStyleSheet("background-color: #555555; color: #EEEEEE");
    formLauncher.pushButtonValidation->setStyleSheet("background-color: #555555; color: #EEEEEE");
    formLauncher.pushButtonError->setStyleSheet("background-color: #555555; color: #EEEEEE");
}

void Launcher::setupLabelText(void)
{
    LispSettings * lispSettings = new LispSettings(this, formLauncher.lineEditConfigurationFile->text().toStdString().c_str());

    formLauncher.labelInputNeurons->setText("Input neurons: " + lispSettings->value("N-INPUT-LAYER"));
    formLauncher.labelHiddenNeurons->setText("Hidden neurons: " + lispSettings->value("N-HIDDEN-LAYER"));
    formLauncher.labelHiddenLayers->setText("Hidden layers: " + lispSettings->value("HIDDEN-LAYERS"));
    formLauncher.labelOutputNeurons->setText("Output neurons: " + lispSettings->value("N-OUTPUT-LAYER"));

    delete lispSettings;
    lispSettings = nullptr;
}


void Launcher::closeEvent(QCloseEvent *event)
{
	// store GUI setup
    this->configurationFile = formLauncher.lineEditConfigurationFile->text();

    for (Launcher* launcher : qlLauncherList)
    {
        QString launcherWindowTitle = "&" + launcher->windowTitle();
        QString thisLauncherWindowTitle = "&" + this->windowTitle();

        if (launcherWindowTitle == thisLauncherWindowTitle)
        {
            qlLauncherList.removeOne(launcher);

            QList<QAction*> actionList = menuWindows->actions();
            for (QAction* action: actionList)
            {
                QString actionText = action->text();
                if (launcherWindowTitle == action->text())
                {
                    menuWindows->removeAction(action);
                    menuWindows->exec();
                }
            }
        }
    }

    LauncherWidget::closeEvent(event);
}

void Launcher::slotStartSimulation(void)
{
    startSimulation();
}

void Launcher::startSimulation(void)
{
    // if no process runs
    if (!simulationProcess)
    {
        formLauncher.progressBar->reset();
        formLauncher.progressBar->setRange(0, 3);
        formLauncher.progressBar->setValue(1);

        QFile outputFile(HOME_DIR + "/output.txt");
        QFile errorFile(HOME_DIR + "/error.txt");

        outputFile.remove();
        errorFile.remove();

    #ifdef ECL_BUILD
        if (!lispEngine)
        {
            lispEngine = new ECL::LispEngine(configurationFile.toStdString().c_str());
        }
        lispEngine->Start();

        QString consoleOutput = lispEngine->getOutput().c_str();
        formLauncher.textEditConsole->setText(consoleOutput);
    #else
        QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
        QString program = "sh";
        QStringList arguments;
        arguments << HOME_DIR + "/start.sh" << HOME_DIR << configurationFile;

        simulationProcess = new QProcess(this);
        simulationProcess->setStandardErrorFile(errorFile.fileName());
        simulationProcess->setStandardOutputFile(outputFile.fileName());
        connect(simulationProcess , SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(processFinished(int, QProcess::ExitStatus)));
        simulationProcess->setProgram(program);
        simulationProcess->setArguments(arguments);
        simulationProcess->setProcessEnvironment(env);
        simulationProcess->setWorkingDirectory(HOME_DIR);
        simulationProcess->start();
        simulationProcess->setProcessChannelMode(QProcess::MergedChannels);
#endif
        formLauncher.progressBar->setValue(2);
    }
}

void Launcher::processFinished(int, QProcess::ExitStatus)
{
    QString content;
    QFile outputFile(HOME_DIR + "/output.txt");
    QFile errorFile(HOME_DIR + "/error.txt");

    formLauncher.progressBar->setValue(3);

    // show images in frame
    showImages();

    // update images in open views
    if (validationImageView)
        validationImageView->updateView();
    if (errorImageView)
        errorImageView->updateView();

    if (errorFile.open(QFile::ReadOnly | QFile::Text))
    {
        QTextStream in(&errorFile);
        content.append(in.readAll());
    }
    if (outputFile.open(QFile::ReadOnly | QFile::Text))
    {
        QTextStream in(&outputFile);
        content.append(in.readAll());
    }
    formLauncher.progressBar->reset();
    formLauncher.textEditConsole->setText(content);
    formLauncher.textEditConsole->repaint();

    // cleanup
    delete simulationProcess;
    simulationProcess = nullptr;
}

void Launcher::showImages(void)
{
    try
    {
        if(!formLauncher.lineEditConfigurationFile->text().isEmpty())
            this->configurationFile = formLauncher.lineEditConfigurationFile->text();

        QString imageFileDir;
        QString imageExt;
        LispSettings* lispSettings = new LispSettings(this, configurationFile);
        QString validationImagePath = getValidationImagePath(lispSettings);
        QString errorImagePath = getErrorImagePath(lispSettings);

        QImage validationImage(validationImagePath);
        QImage errorImage(errorImagePath);

        if (validationScene)
            delete validationScene;
        if (errorScene)
            delete errorScene;
        validationScene = new QGraphicsScene(this);
        errorScene = new QGraphicsScene(this);
        validationScene->addPixmap(QPixmap::fromImage(validationImage).scaledToWidth(340, Qt::SmoothTransformation).scaledToHeight(340, Qt::SmoothTransformation));
        errorScene->addPixmap(QPixmap::fromImage(errorImage).scaledToWidth(340, Qt::SmoothTransformation).scaledToHeight(340, Qt::SmoothTransformation));
        validationScene->setSceneRect(validationImage.rect());
        errorScene->setSceneRect(errorImage.rect());

        formLauncher.graphicsViewValidation->setScene(validationScene);
        formLauncher.graphicsViewError->setScene(errorScene);
        formLauncher.graphicsViewValidation->viewport()->update();
        formLauncher.graphicsViewError->viewport()->update();
    }
    catch(...)
    {
        QMessageBox::question(
            this, "Voids",
            QObject::tr("Can't image or path not found!"),
            QMessageBox::Ok,
            QMessageBox::Ok);
    }
    delete lispSettings;
    lispSettings = nullptr;
}

void Launcher::slotStopSimulation(void)
{
    stopSimulation();
}

void Launcher::stopSimulation(void)
{
#ifdef ECL_BUILD
    if (lispEngine)
    {
        QString consoleOutput = lispEngine->getOutput().c_str();
        formLauncher.textEditConsole->setText(consoleOutput);
    }
#else
    if (simulationProcess)
    {

        simulationProcess->kill();
    }
#endif
}

void Launcher::slotCloseSimulation(void)
{
    this->close();
}

void Launcher::setupConfigurationFile(void)
{
    formLauncher.lineEditConfigurationFile->setText(currentApplicationSettings.qsConfigurationFile);
    previousFolder = currentApplicationSettings.qsConfigurationFile;
    setupLabelText();
}

void Launcher::slotPressConfigurationButton()
{
	QFileDialog fileDialog;
    fileDialog.setDirectory(previousFolder);
    QString fileName = fileDialog.getOpenFileName();
    if (!fileName.isEmpty())
    {
        this->configurationFile = fileName;
        formLauncher.lineEditConfigurationFile->setText(this->configurationFile);
        setupLabelText();
        previousFolder = fileName;
    }
}

QString Launcher::getValidationImagePath(LispSettings* settings)
{
    QString imageFileDir;
    QString imageExt;
    QString validationImagePath;
    imageExt = settings->value("IMG-EXT");
    imageFileDir = settings->value("IMAGEFILES-DIR");
    validationImagePath = imageFileDir + settings->value("VALIDATION-PLOT") + imageExt;
    return validationImagePath;
}

QString Launcher::getErrorImagePath(LispSettings* settings)
{
    QString imageFileDir;
    QString imageExt;
    QString errorImagePath;
    imageExt = settings->value("IMG-EXT");
    imageFileDir = settings->value("IMAGEFILES-DIR");
    errorImagePath = imageFileDir + settings->value("ERROR-PLOT") + imageExt;
    return errorImagePath;
}

void Launcher::slotSelectImageWindow(QAction* action)
{
    QString validationWindowTitle;
    QString errorWindowTitle;
    if (validationImageView)
    {
        validationWindowTitle = "&" + validationImageView->windowTitle();
        QString actionText = action->text();
        if (validationWindowTitle == actionText)
        {
            validationImageView->setHidden(false);
            validationImageView->raise();
            validationImageView->setFocus();
            validationImageView->activateWindow();
        }
    }
    if (errorImageView)
    {
        errorWindowTitle = "&" + errorImageView->windowTitle();
        QString actionText = action->text();
        if (errorWindowTitle == actionText)
        {
            errorImageView->setHidden(false);
            errorImageView->raise();
            errorImageView->setFocus();
            errorImageView->activateWindow();
        }
    }
}

void Launcher::hideImageViews(void)
{
    if (validationImageView)
        validationImageView->setHidden(true);
    if (errorImageView)
        errorImageView->setHidden(true);
 }

void Launcher::slotShowValidationImage(void)
{  
    LispSettings* lispSettings = new LispSettings(this, configurationFile);
    QString validationImagePath = getValidationImagePath(lispSettings);

    QString menuText = "&" + validationImagePath;
    QAction* action = new QAction(tr(menuText.toStdString().c_str()), this);
    action->setStatusTip("Select Window");
    connect(menuWindows, SIGNAL(triggered(QAction*)), this, SLOT(slotSelectImageWindow(QAction*)));
    menuWindows->addAction(action);

    if (validationImageView)
    {
        delete validationImageView;
        validationImageView = nullptr;
    }
    validationImageView = new ImageView(nullptr, validationImagePath, menuWindows);
#ifndef Q_OS_WIN
    ((MainWindow*)mainWindow)->addSubWindow(validationImageView, IMAGE_WINDOW_SIZE_MAX);
#endif
    validationImageView->setWindowTitle(validationImagePath);
    validationImageView->show();
    delete lispSettings;
    lispSettings = nullptr;
}

void Launcher::slotShowErrorImage(void)
{
    LispSettings* lispSettings = new LispSettings(this, configurationFile);
    QString errorImagePath = getErrorImagePath(lispSettings);

    QString menuText = "&" + errorImagePath;
    QAction* action = new QAction(tr(menuText.toStdString().c_str()), this);
    action->setStatusTip("Select Window");
    connect(menuWindows, SIGNAL(triggered(QAction*)), this, SLOT(slotSelectImageWindow(QAction*)));
    menuWindows->addAction(action);

    if (errorImageView)
    {
        delete errorImageView;
        errorImageView = nullptr;
    }
    errorImageView = new ImageView(nullptr, errorImagePath, menuWindows);
#ifndef Q_OS_WIN
    ((MainWindow*)mainWindow)->addSubWindow(errorImageView, IMAGE_WINDOW_SIZE_MAX);
#endif
    errorImageView->setWindowTitle(errorImagePath);
    errorImageView->show();
    delete lispSettings;
    lispSettings = nullptr;
}

void Launcher::openValidationImageExtern(void)
{
      if (!validationImageViewProcess)
          validationImageViewProcess = new QProcess(this);

      LispSettings* lispSettings = new LispSettings(this, configurationFile);
      QString validationImagePath = getValidationImagePath(lispSettings);
      QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
      QFile validationFile(validationImagePath);
      QStringList arguments;
      arguments << validationImagePath;

      ImageView* view = new ImageView(nullptr, validationImagePath);
      view->show();

      if (validationImageViewProcess && validationFile.exists())
      {
          if (validationImagePath.lastIndexOf(".png") >= 0)
              validationImageViewProcess->setProgram(PICTURE_VIEWER);
          else if (validationImagePath.lastIndexOf(".svg") >= 0)
                  validationImageViewProcess->setProgram(SVG_VIEWER);
          validationImageViewProcess->setArguments(arguments);
          validationImageViewProcess->setProcessEnvironment(env);
          validationImageViewProcess->start();
          validationImageViewProcess->setProcessChannelMode(QProcess::MergedChannels);
          validationImageViewProcess->waitForFinished();
          delete validationImageViewProcess;
          validationImageViewProcess = nullptr;
      }
      delete lispSettings;
      lispSettings = nullptr;
}

void Launcher::openErrorImageExtern(void)
{
      if (!errorImageViewProcess)
          errorImageViewProcess = new QProcess(this);

      LispSettings* lispSettings = new LispSettings(this, configurationFile);
      QString errorImagePath = getErrorImagePath(lispSettings);
      QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
      QFile errorFile(errorImagePath);
      QStringList arguments;
      arguments << errorImagePath;

      ImageView* view = new ImageView(nullptr, errorImagePath);
      view->show();

      if (errorImageViewProcess && errorFile.exists())
      {
          if (errorImagePath.lastIndexOf(".png") >= 0)
              errorImageViewProcess->setProgram(PICTURE_VIEWER);
          else if (errorImagePath.lastIndexOf(".svg") >= 0)
                  errorImageViewProcess->setProgram(SVG_VIEWER);
          errorImageViewProcess->setArguments(arguments);
          errorImageViewProcess->setProcessEnvironment(env);
          errorImageViewProcess->start();
          errorImageViewProcess->setProcessChannelMode(QProcess::MergedChannels);
          errorImageViewProcess->waitForFinished();
          delete errorImageViewProcess;
          errorImageViewProcess = nullptr;
      }
      delete lispSettings;
      lispSettings = nullptr;
}


