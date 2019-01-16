using System;
using System.Text;
using RDotNet;
using System.IO;
using System.Collections.ObjectModel;

namespace SeasonPredictionProject
{
    class SeasonPredictor : IDisposable
    {
        private string path;
        private REngine engine;

        public SeasonPredictor(string pathToDate)
        {
            SetupPath();
            path = pathToDate;
            engine = REngine.GetInstance();
            engine.Initialize();
        }

        public ObservableCollection<Parameter> GetFeatures()
        {
            engine.Evaluate(string.Format("dataset <- read.csv('{0}{1}{2}')", path, "//", "archive.csv"));
            engine.Evaluate(string.Format("source('{0}{1}{2}')", path, "//", "DataAnalysis.R"));
            var names = engine.Evaluate(string.Format("colnames(dataset[4:7])")).AsCharacter().ToArray();
            var parameters = new ObservableCollection<Parameter>();
            foreach (var name in names)
            {
                var min = engine.Evaluate(string.Format("min(dataset${0})", name)).AsNumeric().ToArray()[0];
                var max = engine.Evaluate(string.Format("max(dataset${0})", name)).AsNumeric().ToArray()[0];
                parameters.Add(new Parameter(name, max, min));
            }
            return parameters;
        }

        public string Predict(params string[] parameters)
        {
            var airParameters = new StringBuilder("data.frame(");
            foreach (var param in parameters)
                airParameters.Append(param + ",");
            airParameters.Append(")");
            engine.Evaluate(string.Format("air.parameters <- {0}", airParameters.ToString()));
            engine.Evaluate("names(air.parameters) <- colnames(data[4:7])");
            engine.Evaluate("predictions <- predict(fit.lda, air.parameters)");
            return engine.Evaluate("as.character(predictions)").AsCharacter().ToArray()[0];
        }

        public static void SetupPath()
        {
            var oldPath = Environment.GetEnvironmentVariable("PATH");
            var rPath1 = @"C:\Program Files\Microsoft\R Client\R_SERVER\bin\x64";
            var rPath2 = @"C:\Program Files\Microsoft\R Client\R_SERVER\library\caret\R";
            if (!Directory.Exists(rPath1)) throw new DirectoryNotFoundException(string.Format(" R.dll not found in : {0}", rPath1));
            var newPath = string.Format("{0}{1}{2}", rPath1, Path.PathSeparator, oldPath);
            newPath = string.Format("{0}{1}{2}", rPath2, Path.PathSeparator, newPath);
            Environment.SetEnvironmentVariable("PATH", newPath);
        }

        public void Dispose() => engine.Dispose();
    }
}
