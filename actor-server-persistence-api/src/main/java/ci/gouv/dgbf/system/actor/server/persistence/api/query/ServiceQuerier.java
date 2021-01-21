package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.annotation.Queries;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.security.keycloak.server.Client;
import org.cyk.utility.security.keycloak.server.ClientManager;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Service;

@Queries(value = {
		@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Service.class,name = FunctionQuerier.QUERY_NAME_READ,value = "SELECT t FROM Service t ORDER BY t.code ASC")
	})
public interface ServiceQuerier extends Querier.CodableAndNamable<Service> {

	/**/
	
	/* read order by code ascending */
	String QUERY_NAME_READ = "readOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(Service.class, QUERY_NAME_READ);
	Collection<Service> read(QueryExecutorArguments arguments);
	String QUERY_IDENTIFIER_COUNT = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ);
	Long count();
	
	/* read with all order by code ascending */
	String QUERY_IDENTIFIER_READ_WITH_ALL = QueryIdentifierBuilder.getInstance().build(Service.class, "readWithAll");
	Collection<Service> readWithAll(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_BY_CODE = QueryIdentifierBuilder.getInstance().build(Service.class, "readByCode");
	Service readByCode(String code);
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Service> implements ServiceQuerier,Serializable {
		
		@Override
		public Collection<Service> read(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ);
			return QueryExecutor.getInstance().executeReadMany(Service.class,arguments);
		}
		
		@Override
		public Long count() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT);
		}
		
		@Override
		public Collection<Service> readWithAll(QueryExecutorArguments arguments) {
			Collection<Service> services = read(arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ));
			if(CollectionHelper.isEmpty(services))
				return null;
			__setAll__(services);
			return services;
		}
		
		@Override
		public Collection<Service> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ.equals(arguments.getQuery().getIdentifier()))
				return read(arguments);
			if(QUERY_IDENTIFIER_READ_WITH_ALL.equals(arguments.getQuery().getIdentifier()))
				return readWithAll(arguments);
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT.equals(arguments.getQuery().getIdentifier()))
				return count();
			return super.count(arguments);
		}
		
		@Override
		public Service readByCode(String code) {
			return QueryExecutor.getInstance().executeReadOne(Service.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_CODE)
					.addFilterField(PARAMETER_NAME_CODE, code));
		}
		
		@Override
		protected Class<Service> getKlass() {
			return Service.class;
		}
		
		private static void __setAll__(Collection<Service> services) {
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
					service.setStatus(Boolean.TRUE.equals(service.getSecured()) ? "Privé" : "Public");
					service.setNumberOfMenusSecured(CollectionHelper.getSize(client.getResources()));
					service.setNumberOfMenusSecuredAsString(NumberHelper.getInteger(service.getNumberOfMenusSecured())+"/"+NumberHelper.getInteger(service.getNumberOfMenus()));
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
		QueryHelper.addQueries(
				Query.buildSelect(Service.class, QUERY_IDENTIFIER_READ_BY_CODE, "SELECT t FROM Service t WHERE t.code = :"+PARAMETER_NAME_CODE)			
			);
	}
}