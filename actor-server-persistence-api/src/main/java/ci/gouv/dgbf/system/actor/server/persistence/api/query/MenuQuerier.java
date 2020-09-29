package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

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
import org.cyk.utility.__kernel__.security.keycloak.Resource;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Menu;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Service;

@Queries(value = {
		@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Menu.class,name = MenuQuerier.QUERY_NAME_READ,value = "SELECT t FROM Menu t ORDER BY t.code ASC")
	})
public interface MenuQuerier extends Querier.CodableAndNamable<Menu> {

	/**/
	
	/* read order by code ascending */
	String QUERY_NAME_READ = "readOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(Menu.class, QUERY_NAME_READ);
	Collection<Menu> read();
	String QUERY_IDENTIFIER_COUNT = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ);
	Long count();
	
	/* read with all order by code ascending */
	String QUERY_IDENTIFIER_READ_WITH_ALL = QueryIdentifierBuilder.getInstance().build(Menu.class, "readWithAll");
	Collection<Menu> readWithAll();
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Menu> implements MenuQuerier,Serializable {
		
		@Override
		public Collection<Menu> read() {
			return QueryExecutor.getInstance().executeReadMany(Menu.class,QUERY_IDENTIFIER_READ);
		}
		
		@Override
		public Long count() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT);
		}
		
		@Override
		public Collection<Menu> readWithAll() {
			Collection<Menu> menus = read();
			if(CollectionHelper.isEmpty(menus))
				return null;
			__setAll__(menus);
			return menus;
		}
		
		@Override
		public Collection<Menu> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ.equals(arguments.getQuery().getIdentifier()))
				return read();
			if(QUERY_IDENTIFIER_READ_WITH_ALL.equals(arguments.getQuery().getIdentifier()))
				return readWithAll();
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT.equals(arguments.getQuery().getIdentifier()))
				return count();
			return super.count(arguments);
		}
		
		@Override
		protected Class<Menu> getKlass() {
			return Menu.class;
		}
		
		private static void __setAll__(Collection<Menu> menus) {
			if(CollectionHelper.isEmpty(menus))
				return;
			Collection<Service> services = menus.stream().filter(menu -> menu.getService() != null).map(menu -> menu.getService()).collect(Collectors.toList());
			if(CollectionHelper.isEmpty(services))
				return;
			Collection<Client> clients = ClientManager.getInstance().getByIdentifiers(FieldHelper.readBusinessIdentifiersAsStrings(services));
			if(CollectionHelper.isEmpty(clients))
				return;
			for(Menu menu : menus) {
				Resource resource = null;
				for(Client client : clients) {
					if(CollectionHelper.isEmpty(client.getResources()))
						continue;
					for(Resource resourceIndex : client.getResources()) {
						if(CollectionHelper.isEmpty(resourceIndex.getUniformResourceIdentifiers()))
							continue;
						if(resourceIndex.getUniformResourceIdentifiers().contains(menu.getUniformResourceIdentifier())) {
							resource = resourceIndex;
							break;
						}
					}
				}
				if(resource == null) {
					
				}else {
					menu.setDefined(Boolean.TRUE);
				}
			}
		}
	}
	
	/**/
	
	static MenuQuerier getInstance() {
		return Helper.getInstance(MenuQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(Menu.class);
	}
}