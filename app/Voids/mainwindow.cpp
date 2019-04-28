#include <QWindow>
#include <QMessageBox>
#include <QShowEvent>
#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "ConfigurationWidget.h"
#include "LauncherWidget.h"

/*************************************** Const *************************************/

QList<Launcher*> qlLauncherList;

#ifdef Q_OS_WIN
const QSize LAUNCHER_WINDOW_SIZE_MAX(780,800);
#endif

#ifdef Q_OS_UNIX
const QSize LAUNCHER_WINDOW_SIZE_MAX(780,820);
#endif

/*************************************** Implementation *****************************/

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    this->setStyleSheet(("background-color: #444444"));
    settingsManager = new SettingsManager(HOME_DIR + "Voids.ini");
    instanceCnt = 1;
    isInitialized = false;
}

MainWindow::~MainWindow()
{
    for (auto child : qlLauncherList)
    {
        delete child;
    }
    qlLauncherList.clear();
    ui->mdiAreaLauncher->closeAllSubWindows();
    delete ui;
}

void MainWindow::showEvent(QShowEvent *event)
{
    QWidget::showEvent(event);
    if (event->spontaneous())
        return;

    if (isInitialized)
        return;

    isInitialized = true;
    loadSettings();
}

void MainWindow::closeEvent(QCloseEvent *event)
{
    QMessageBox msg;
    QMessageBox::StandardButton resBtn = msg.question(
        this, "Voids",
        tr("Are you sure?\n"),
        QMessageBox::Cancel | QMessageBox::No | QMessageBox::Yes,
        QMessageBox::Yes);
    if (resBtn != QMessageBox::Yes) {
        event->ignore();
    }
    else {
        event->accept();
        writeSettings();
    }
}

void MainWindow::slotCloseSimulation(void)
{
    removeSimulation();
}

void MainWindow::slotNewSimulation()
{
    addNewSimulation();
}

void MainWindow::slotQuit()
{
    this->close();
}

void MainWindow::slotPreferences()
{
    config = new ConfigurationWidget();
    Launcher* currentLauncher = getCurrentLauncher();
    if (currentLauncher)
    {
        config->setConfigurationFile(currentLauncher->getConfigurationFile());
        config->show();
    }
}

void MainWindow::slotClosePreferences(void)
{
    Launcher* currentLauncher = getCurrentLauncher();
    if (currentLauncher)
    {
        currentLauncher->setConfigurationFile(config->filePath());
        currentLauncher->setupConfigurationFile();
        delete config;
    }
}

void MainWindow::slotStartSimulation(void)
{
    Launcher* launcher = getCurrentLauncher();
    if (launcher)
    {
        launcher->slotStartSimulation();
    }
}

void MainWindow::slotStopSimulation(void)
{
    Launcher* launcher = getCurrentLauncher();
    if (launcher)
    {
        launcher->slotStopSimulation();
    }
}

void MainWindow::slotSelectWindow(QAction* action)
{
    for (Launcher* launcher : qlLauncherList)
    {
        QString launcherWindowTitle = "&" + launcher->windowTitle();
        QString actionText = action->text();
        if (launcherWindowTitle == actionText)
        {
            launcher->raise();
            launcher->setFocus();
            launcher->activateWindow();
            //launcher->hideImageViews();
        }
    }
}

QMdiSubWindow* MainWindow::addSubWindow(QWidget* widget, QSize size)
{
    QMdiSubWindow* subWindow = ui->mdiAreaLauncher->addSubWindow(
        widget,
        Qt::WindowFlags::enum_type::Widget);
    subWindow->resize(size);
    subWindow->setFixedSize(size);
    return subWindow;
}

Launcher* MainWindow::addNewSimulation()
{
    QString windowTitle = QString("%1 %2").arg("VOIDS", QString::number(instanceCnt));
    instanceCnt++;

    Launcher* launcherWidget = new Launcher(this, ui->menuWindows);
    if (launcherWidget->isAborted() == false)
    {
        launcherWidget->setWindowTitle(windowTitle);
        launcherWidget->resize(LAUNCHER_WINDOW_SIZE_MAX);
        launcherWidget->setFixedSize(LAUNCHER_WINDOW_SIZE_MAX);
#ifndef Q_OS_WIN
        addSubWindow(launcherWidget, LAUNCHER_WINDOW_SIZE_MAX);
#endif

#ifdef Q_OS_WIN
        launcherWidget->setWindowFlags(Qt::WindowFlags::enum_type::Dialog);
#endif
        launcherWidget->show();
        qlLauncherList.push_front(launcherWidget);

        QString menuText = "&" + windowTitle;
        QAction* action = new QAction(tr(menuText.toStdString().c_str()), this);
        action->setStatusTip("Select Window");
        connect(ui->menuWindows, SIGNAL(triggered(QAction*)), this, SLOT(slotSelectWindow(QAction*)));
        ui->menuWindows->addAction(action);

        return launcherWidget;
    }
    return nullptr;
}

void MainWindow::removeSimulation(void)
{
    QMdiSubWindow* subWindow = (QMdiSubWindow*)ui->mdiAreaLauncher->activeSubWindow();
    Launcher* launcher = getCurrentLauncher();
    if (launcher)
    {
        qlLauncherList.removeOne(launcher);
        QString launcherWindowTitle = "&" + launcher->windowTitle();

        QList<QAction*> actionList = ui->menuWindows->actions();
        for (QAction* action: actionList)
        {
            if (launcherWindowTitle == action->text())
            {
                ui->menuWindows->removeAction(action);
                ui->menuWindows->exec();
            }
        }
        if (subWindow)
        {
            subWindow->close();
            delete subWindow;
        }
    }
}

void MainWindow::loadSettings()
{
    QList<ApplicationSettings> settings = settingsManager->loadApplicationSettings();
    if (settingsManager->isMaximized())
        this->setWindowState(Qt::WindowState::WindowMaximized);

    for (ApplicationSettings set : settings)
    {
        auto child = addNewSimulation();
        if (child)
            setupLauncherSettings(set, child);
    }
}

void MainWindow::setupLauncherSettings(ApplicationSettings& setting, Launcher* widget)
{
    if (widget)
    {
        QMdiSubWindow* window = (QMdiSubWindow*)widget->parent();
        window->move(setting.qptWindowPostion.x(), setting.qptWindowPostion.y());
        window->setWindowTitle(setting.qsInstanceName);
        widget->setCurrentApplicationSettings(setting);
        widget->setupConfigurationFile();
    }
}

void MainWindow::writeSettings()
{
    int idCnt = 0;
    settingsManager->resetSettings();
    for (auto child : qlLauncherList)
    {
        if (child)
        {
            ApplicationSettings appSetting;
            appSetting.iInstanceID = ++idCnt;
            appSetting.qsInstanceName = child->windowTitle();
            appSetting.qsConfigurationFile = child->getLauncher().lineEditConfigurationFile->text();
            appSetting.qptWindowPostion.setX(child->parentWidget()->x());
            appSetting.qptWindowPostion.setY(child->parentWidget()->y());
            settingsManager->appendSetting(appSetting);
        }
    }
    settingsManager->setMaximized(this->isMaximized());
    settingsManager->saveApplicationSettings();
}

Launcher* MainWindow::getCurrentLauncher(void)
{
    Launcher*  launcher = nullptr;

    QMdiSubWindow* subWindow = (QMdiSubWindow*)ui->mdiAreaLauncher->activeSubWindow();
    if (subWindow)
    {
        auto ptr = subWindow->widget();
        if (dynamic_cast<Launcher*>(ptr))
        {
            launcher = dynamic_cast<Launcher*>(ptr);
        }
    }
    return launcher;
}

