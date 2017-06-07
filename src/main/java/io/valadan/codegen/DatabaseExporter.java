package io.valadan.codegen;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.hibernate.boot.registry.StandardServiceRegistryBuilder;
import org.hibernate.boot.registry.internal.StandardServiceRegistryImpl;
import org.hibernate.cfg.JDBCMetaDataConfiguration;
import org.hibernate.cfg.JDBCReaderFactory;
import org.hibernate.cfg.Settings;
import org.hibernate.cfg.reveng.DatabaseCollector;
import org.hibernate.cfg.reveng.DefaultReverseEngineeringStrategy;
import org.hibernate.cfg.reveng.JDBCReader;
import org.hibernate.cfg.reveng.MappingsDatabaseCollector;
import org.hibernate.cfg.reveng.ReverseEngineeringStrategy;
import org.hibernate.engine.jdbc.connections.spi.ConnectionProvider;
import org.hibernate.engine.jdbc.spi.JdbcServices;
import org.hibernate.service.ServiceRegistry;
import org.hibernate.service.spi.Stoppable;

public class DatabaseExporter {
	private ReverseEngineeringStrategy revengStrategy = new DefaultReverseEngineeringStrategy();
	private ServiceRegistry serviceRegistry = null;
	private JDBCMetaDataConfiguration cfg = new JDBCMetaDataConfiguration();

	public DatabaseExporter(Properties properties) {
		this.cfg.setProperties(properties);
	}

	public DatabaseExporter(JDBCMetaDataConfiguration cfg) {
		this.cfg=cfg;
	}
	
	public static void main(String[] args) throws IOException {
		Properties properties = new Properties();
		InputStream is = Thread.currentThread().getContextClassLoader().getResourceAsStream("database.properties");
		properties.load(is);
		DatabaseExporter exporter = new DatabaseExporter(properties);
		exporter.export();
	}
	
	public DatabaseCollector export(){

		try {
			JDBCReader reader = JDBCReaderFactory.newJDBCReader(cfg.getProperties(), buildSettings(), revengStrategy, cfg.getServiceRegistry());
			DatabaseCollector dbs = new MappingsDatabaseCollector(cfg.createMappings(), reader.getMetaDataDialect());
		    reader.readDatabaseSchema(dbs, null, "openvbx");		    
		    return dbs;		
		} 
		finally	{
			ConnectionProvider connectionProvider = cfg.getServiceRegistry().getService(JdbcServices.class).getConnectionProvider();
			if ( connectionProvider instanceof Stoppable ) {
				( ( Stoppable ) connectionProvider ).stop();
			}
		}
	}
	
	public Settings buildSettings(ServiceRegistry serviceRegistry) {
		this.serviceRegistry = serviceRegistry;
		return cfg.buildSettings(serviceRegistry);
	}
	
	public Settings buildSettings() {
		destroyServiceRegistry();
		return buildSettings( getServiceRegistry() );
	}
	
	public ServiceRegistry getServiceRegistry(){
		if(serviceRegistry == null){
			serviceRegistry = new StandardServiceRegistryBuilder()
				.applySettings(cfg.getProperties())
				.build();
		}
		return serviceRegistry;
	}
	
	private void destroyServiceRegistry(){
		if (serviceRegistry instanceof StandardServiceRegistryImpl) {
			( (StandardServiceRegistryImpl) serviceRegistry ).destroy();
		}
		serviceRegistry = null;
	}
	
}
