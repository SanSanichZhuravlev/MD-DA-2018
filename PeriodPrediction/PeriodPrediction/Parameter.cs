﻿using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;

namespace PeriodPrediction
{
    class Parameter : INotifyPropertyChanged
    {
        private double val;

        public string Name { get; set; }
        public double Min { get; set; }
        public double Max { get; set; }

        public double Value
        {
            get { return val; }
            set
            {
                if (value > Max || value < Min) throw new ArgumentException();
                val = value;
                OnPropertyChanged("Value");
            }
        }

        public void Reset()
        {
            Value = (Min + Max) / 2;
        }

        public Parameter(string name, double max, double min)
        {
            Name = name;
            Min = min;
            Max = max;
            Reset();
        }

        public event PropertyChangedEventHandler PropertyChanged;
        public void OnPropertyChanged([CallerMemberName]string prop = "")
        {
            if (PropertyChanged != null)
                PropertyChanged(this, new PropertyChangedEventArgs(prop));
        }
    }
}
