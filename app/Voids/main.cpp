#include "mainwindow.h"
#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    a.setStyleSheet("QPushButton { background-color: #555555; color: white } \
                    QWidget { background-color: #222222; color: white } \
                    QtTreePropertyBrowser { background-color: #222222; color: #444444 } \
                    QMainWindow { background-color: #333333; color: white } \
                    QMdiArea { background-color: #555555; color: white } \
                    QGridLayout { background-color: #222222 } \
                    QLineEdit { background-color: #222222; color: white } \
                    QGroupBox { background-color: #333333; color: #EEEEEE } \
                    QGroupBox::title { background-color: transparent; color: #EEEEEE } \
                    QMessageBox { background-color: #555555; color: #FFFFFF } \
                    QScrollBar { background-color: #555555; color: #444444 } \
                    QScrollArea { background-color: #555555; color: #444444 } \
                    QGraphicsView { background-color: #FFFFFF } \
                    QFrame { background-color: #FFFFFF; color: black } \
                    QTextEdit { background-color: #FFFFFF; color: black } \
                    QLabel { color: #EEEEEE }");
    MainWindow w;
    w.show();

    return a.exec();
}
