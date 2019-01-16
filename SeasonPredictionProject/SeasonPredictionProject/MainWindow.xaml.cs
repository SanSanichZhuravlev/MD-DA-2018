using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Windows;

namespace SeasonPredictionProject
{
    /// <summary>
    /// Логика взаимодействия для MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        ObservableCollection<Parameter> parameters;
        SeasonPredictor predictor;

        public MainWindow()
        {
            InitializeComponent();

            predictor = new SeasonPredictor("..//..//Data");
            parameters = predictor.GetFeatures();
            featuresGrid.ItemsSource = parameters;
        }

        private void Predict_Click(object sender, RoutedEventArgs e)
        {
            var values = new List<string>();
            foreach (var feature in parameters)
                values.Add(feature.Value.ToString());
            quality.Content = predictor.Predict(values.ToArray());
        }

        private void Reset_Click(object sender, RoutedEventArgs e)
        {
            foreach (var parameter in parameters)
                parameter.Reset();
        }
    }
}
