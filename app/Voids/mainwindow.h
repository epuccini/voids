#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QShowEvent>
#include <QCloseEvent>
#include <QList>
#include <QMdiSubWindow>

#include "ConfigurationWidget.h"
#include "Launcher.h"

#ifdef Q_OS_WIN
    const QString HOME_DIR = "c:\\Program Files\\Voids\\bin\\";
#endif

#ifdef Q_OS_UNIX
    const QString HOME_DIR = "/Applications/Voids.app/Contents/bin/";
//    const QString HOME_DIR = "/usr/local/bin/";
#endif

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

    void loadSettings();
    void writeSettings();
    Launcher* getCurrentLauncher(void);
    Launcher* addNewSimulation(void);
    void removeSimulation(void);
    QMdiSubWindow* addSubWindow(QWidget* widget, QSize size);
    void showEvent(QShowEvent *event);
    void closeEvent(QCloseEvent *event);

public slots:
    void slotNewSimulation();
    void slotStartSimulation(void);
    void slotStopSimulation(void);
    void slotCloseSimulation();
    void slotQuit();
    void slotPreferences();
    void slotClosePreferences();
    void slotSelectWindow(QAction* action);

private:
    void setupLSimulation(ApplicationSettings& setting, Launcher* widget);
    void setupLauncherSettings(ApplicationSettings& setting, Launcher* widget);

    int instanceCnt;
    SettingsManager* settingsManager;
    bool isInitialized;
    Ui::MainWindow *ui;
    ConfigurationWidget* config;
};

#endif // MAINWINDOW_H
