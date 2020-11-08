using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml;
using System.Xml.Linq;

namespace KmlParser
{
    public class Program
    {
        static int Main(string[] args)
        {
            var path = args.FirstOrDefault();
            if (path == null)
            {
                Console.WriteLine("Drop the .kml file onto the .exe to initiate.");
                Console.ReadKey();
                return 1;
            }

            var kmlFile = new DirectoryInfo(path);

            if (!kmlFile.Extension.Equals(".kml"))
            {
                Console.WriteLine("The provided file is not a .kml.");
                Console.ReadKey();
                return 2;
            }

            var csvFile = Path.Combine(kmlFile.Parent.FullName, "Zinj.csv");

            var parsedData = new KmlParser().Parse(kmlFile.FullName);

            new CsvBuilder().Build(parsedData, csvFile);

            return 0;
        }
    }

    public class KmlParser
    {
        public List<string> Parse(string path)
        {
            using var readStream = File.OpenRead(path);

            var parsedData = new List<string>();

            var printNextValue = false;
            //flag specifically used to avoid inner boundaries
            var printNextcoords = false;
            using XmlReader reader = XmlReader.Create(readStream, new XmlReaderSettings
            {
                IgnoreComments = false
            });

            while (reader.Read())
            {
                switch (reader.NodeType)
                {
                    case XmlNodeType.Element:
                        switch (reader.Name)
                        {
                            case "name":
                                printNextValue = true;
                                break;
                            case "SimpleData":
                                if (reader.AttributeCount > 0
                                    && reader.GetAttribute("name").Equals("nature"))
                                {
                                    printNextValue = true;
                                }
                                break;
                            case "outerBoundaryIs":
                                printNextcoords = true;
                                break;
                            case "coordinates":
                                //only print outer boundaries
                                if (printNextcoords)
                                {
                                    printNextValue = true;
                                }
                                break;
                        }
                        break;
                    case XmlNodeType.Text:
                        if (printNextValue)
                        {
                            printNextValue = false;
                            printNextcoords = false;

                            if (reader.Value.Equals("Zone_1")
                                || reader.Value.Equals("Zone_2")
                                || reader.Value.Equals("Zone_3"))
                            {
                                //ignore zone names
                                continue;
                            }

                            parsedData.Add(reader.Value);
                        }
                        break;
                    case XmlNodeType.CDATA:
                        var cdata = ParseCdata(reader.Value);
                        parsedData.Add(cdata);
                        break;
                    default:
                        break;
                }
            }

            readStream.Close();
            readStream.Dispose();

            return parsedData;
        }

        public string ParseCdata(string cdataString)
        {
            //enclose in parent to prevent multiple root nodes
            cdataString = $"<parent>{cdataString}</parent>";
            var cdataParent = XElement.Parse(cdataString);
            var cdata = cdataParent.Descendants();
            //disgusting hack that assumes the second i tag is the value of the nature
            return cdata.FirstOrDefault(i => i.Name == "i").Value;
        }
    }

    public class CsvBuilder
    {
        public void Build(List<string> parsedData, string path)
        {
            using var writeStream = File.CreateText(path);

            var count = 0;
            var buildings = new List<Building>();
            while (count < parsedData.Count)
            {
                var name = parsedData[count];

                var type = BuildingType.Unspecified;
                switch (parsedData[count + 1])
                {
                    case "Autre":
                        type = BuildingType.Autre;
                        break;
                    case "Autre/Ecole":
                        type = BuildingType.AutreEcole;
                        break;
                    case "Autre/Eglise":
                        type = BuildingType.AutreEglise;
                        break;
                    case "Autre/Parking":
                        type = BuildingType.AutreParking;
                        break;
                    case "church":
                        type = BuildingType.Church;
                        break;
                    case "collective_house":
                        type = BuildingType.CollectiveHouse;
                        break;
                    case "commercial_building":
                        type = BuildingType.CommercialBuilding;
                        break;
                    case "commercial_building sportive":
                        type = BuildingType.CommercialBuildingSportive;
                        break;
                    case "garage":
                        type = BuildingType.Garage;
                        break;
                    case "hospital":
                        type = BuildingType.Hospital;
                        break;
                    case "light_building":
                        type = BuildingType.LightBuilding;
                        break;
                    case "school":
                        type = BuildingType.School;
                        break;
                    case "single_house":
                        type = BuildingType.SingleHouse;
                        break;
                    case "sport_building":
                        type = BuildingType.SportBuilding;
                        break;
                }

                var lats = new List<double>();
                var logs = new List<double>();
                var coordPairStrings = parsedData[count + 2].Split(" ").ToList();
                foreach (var coordPairString in coordPairStrings)
                {
                    var coordPair = coordPairString.Split(",").ToList();

                    if (double.TryParse(coordPair.First(), out var tmpLat))
                    {
                        lats.Add(tmpLat);
                    }

                    if (double.TryParse(coordPair.First(), out var tmpLog))
                    {
                        logs.Add(tmpLog);
                    }
                }

                buildings.Add(new Building
                {
                    Name = name.Trim(),
                    Type = type,
                    Latitude = lats.Sum() / lats.Count,
                    Longitude = logs.Sum() / logs.Count
                });

                count += 3;
            }

            writeStream.WriteLine("Name,Type,Latitude,Logitude");
            buildings.ForEach(b => writeStream.WriteLine($"{b.Name},{Enum.GetName(typeof(BuildingType), b.Type)},{b.Latitude},{b.Longitude}"));

            writeStream.Flush();
            writeStream.Close();
            writeStream.Dispose();
        }
    }

    public class Building
    {
        public string Name { get; set; }
        public BuildingType Type { get; set; }
        public double Latitude { get; set; }
        public double Longitude { get; set; }
    }

    public enum BuildingType
    {
        Unspecified,
        Autre,
        AutreEcole,
        AutreEglise,
        AutreParking,
        Church,
        CollectiveHouse,
        CommercialBuilding,
        CommercialBuildingSportive,
        Garage,
        Hospital,
        LightBuilding,
        School,
        SingleHouse,
        SportBuilding
    }
}
