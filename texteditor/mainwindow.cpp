#include "mainwindow.h"
#include "./ui_mainwindow.h"
#include <QFile>
#include <QFileSystemModel>
#include <QFileDialog>
#include <QKeyEvent>
#include "tools.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    //this->setCentralWidget(ui->centralwidget);
    QFileSystemModel *model=new QFileSystemModel(this);
    model->setRootPath(QDir::rootPath());
    ui->treeView->setModel(model);
    //ui->treeView->setRootIndex(model->setRootPath(currentFile));
}
void MainWindow::on_treeView_doubleClicked(const QModelIndex &index)
{
    QFile file(model->filePath(index));
    currentFile=model->filePath(index);
    if(file.open(QFile::ReadOnly | QFile::Text))
    {
        QTextStream in(&file);
        QString text = in.readAll();
        content=text;
        ui->plainTextEdit->setPlainText(text);
        file.close();
    }
}

MainWindow::~MainWindow(){
    delete ui;
}
void MainWindow::keyPressEvent(QKeyEvent *event){
    auto k=event->key();

    switch (k) {
        case (Qt::Key_F1):
            {
            auto command=ui->plainTextEdit_2->toPlainText();
            ui->label->setText("comand "+command);
            system(command.toStdString().c_str());
            break;}
        case (Qt::Key_F4):{//new file
            currentFile.clear();
            ui->plainTextEdit->setPlainText(QString());
            break;
        }
        case (Qt::Key_F5):{
            break;
        }
    }

}

void MainWindow::on_actionNew_triggered(){
    currentFile.clear();
    ui->plainTextEdit->setPlainText(QString());
}


void MainWindow::on_actionOpen_triggered()
{
    //QString filename= QFileDialog
}


void MainWindow::on_treeView_clicked(const QModelIndex &index)
{

}

