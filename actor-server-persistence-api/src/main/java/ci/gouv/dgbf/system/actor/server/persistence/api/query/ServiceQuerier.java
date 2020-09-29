package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.annotation.Queries;
import org.cyk.utility.__kernel__.security.keycloak.Client;
import org.cyk.utility.__kernel__.security.keycloak.ClientManager;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Service;

@Queries(value = {
		@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Service.class,name = FunctionQuerier.QUERY_NAME_READ,value = "SELECT t FROM Service t ORDER BY t.code ASC")
	})
public interface ServiceQuerier extends Querier.CodableAndNamable<Service> {

	/**/
	
	/* read order by code ascending */
	String QUERY_NAME_READ = "readOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(Service.class, QUERY_NAME_READ);
	Collection<Service> read();
	String QUERY_IDENTIFIER_COUNT = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ);
	Long count();
	
	/* read with external infos order by code ascending */
	String QUERY_IDENTIFIER_READ_WITH_EXTERNAL_INFOS = QueryIdentifierBuilder.getInstance().build(Service.class, "readWithExternalInfos");
	Collection<Service> readWithExternalInfos();
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Service> implements ServiceQuerier,Serializable {
		
		@Override
		public Collection<Service> read() {
			return QueryExecutor.getInstance().executeReadMany(Service.class,QUERY_IDENTIFIER_READ);
		}
		
		@Override
		public Long count() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT);
		}
		
		@Override
		public Collection<Service> readWithExternalInfos() {
			Collection<Service> services = read();
			if(CollectionHelper.isEmpty(services))
				return null;
			__setExternalInfos__(services);
			return services;
		}
		
		@Override
		public Collection<Service> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ.equals(arguments.getQuery().getIdentifier()))
				return read();
			if(QUERY_IDENTIFIER_READ_WITH_EXTERNAL_INFOS.equals(arguments.getQuery().getIdentifier()))
				return readWithExternalInfos();
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT.equals(arguments.getQuery().getIdentifier()))
				return count();
			return super.count(arguments);
		}
		
		@Override
		protected Class<Service> getKlass() {
			return Service.class;
		}
		
		private static void __setExternalInfos__(Collection<Service> services) {
			if(CollectionHelper.isEmpty(services))
				return;
			Collection<Client> clients = ClientManager.getInstance().getByIdentifiers(FieldHelper.readBusinessIdentifiersAsStrings(services));
			if(CollectionHelper.isEmpty(clients))
				return;
			for(Service service : services) {
				Client client = null;
				for(Client index : clients)
					if(service.getCode().equals(index.getIdentifier())) {
						client = index;
						break;
					}
				if(client == null) {
					
				}else {
					service.setDefined(Boolean.TRUE);
					service.setSecured(client.getSecured());
				}
			}
		}
	}
	
	/**/
	
	static ServiceQuerier getInstance() {
		return Helper.getInstance(ServiceQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(Service.class);
	}
}