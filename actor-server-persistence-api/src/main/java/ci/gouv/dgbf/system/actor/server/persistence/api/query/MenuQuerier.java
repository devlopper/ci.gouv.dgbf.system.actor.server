package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.annotation.Queries;
import org.cyk.utility.security.keycloak.server.Client;
import org.cyk.utility.security.keycloak.server.ClientManager;
import org.cyk.utility.security.keycloak.server.Resource;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Menu;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Service;

@Queries(value = {
		@org.cyk.utility.persistence.annotation.Query(tupleClass = Menu.class,name = MenuQuerier.QUERY_NAME_READ
				,value = "SELECT t FROM Menu t ORDER BY t.code ASC")
		,@org.cyk.utility.persistence.annotation.Query(tupleClass = Menu.class,name = MenuQuerier.QUERY_NAME_READ_BY_SERVICE_IDENTIFIER
			,value = "SELECT t FROM Menu t WHERE t.service.identifier = :"+MenuQuerier.PARAMETER_NAME_SERVICE_IDENTIFIER+" ORDER BY t.code ASC")
		,@org.cyk.utility.persistence.annotation.Query(tupleClass = Menu.class,name = MenuQuerier.QUERY_NAME_READ_BY_SERVICE_CODE
			,value = "SELECT t FROM Menu t WHERE t.service.code = :"+MenuQuerier.PARAMETER_NAME_SERVICE_CODE+" ORDER BY t.code ASC")
	})
public interface MenuQuerier extends Querier.CodableAndNamable<Menu> {

	String PARAMETER_NAME_SERVICE_IDENTIFIER = "serviceIdentifier";
	String PARAMETER_NAME_SERVICE_CODE = "serviceCode";
	
	/**/
	
	/* read order by code ascending */
	String QUERY_NAME_READ = "readOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(Menu.class, QUERY_NAME_READ);
	Collection<Menu> read();
	String QUERY_IDENTIFIER_COUNT = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ);
	Long count();
	
	/* read by service identifier order by code ascending */
	String QUERY_NAME_READ_BY_SERVICE_IDENTIFIER = "readByServiceIdentifierOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ_BY_SERVICE_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Menu.class, QUERY_NAME_READ_BY_SERVICE_IDENTIFIER);
	Collection<Menu> readByServiceIdentifier(String serviceIdentifier);
	String QUERY_IDENTIFIER_COUNT_BY_SERVICE_IDENTIFIER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_SERVICE_IDENTIFIER);
	Long countByServiceIdentifier(String serviceIdentifier);
	
	/* read by service code order by code ascending */
	String QUERY_NAME_READ_BY_SERVICE_CODE = "readByServiceCodeOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ_BY_SERVICE_CODE = QueryIdentifierBuilder.getInstance().build(Menu.class, QUERY_NAME_READ_BY_SERVICE_CODE);
	Collection<Menu> readByServiceCode(String serviceCode);
	String QUERY_IDENTIFIER_COUNT_BY_SERVICE_CODE = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_SERVICE_CODE);
	Long countByServiceCode(String serviceCode);
	
	/* read with all order by code ascending */
	String QUERY_IDENTIFIER_READ_WITH_ALL = QueryIdentifierBuilder.getInstance().build(Menu.class, "readWithAll");
	Collection<Menu> readWithAll();
	
	/* read with all by service identifier order by code ascending */
	String QUERY_IDENTIFIER_READ_WITH_ALL_BY_SERVICE_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Menu.class, "readWithAllByServiceIdentifier");
	Collection<Menu> readWithAllByServiceIdentifier(String serviceIdentifier);
	
	/* read with all by service code order by code ascending */
	String QUERY_IDENTIFIER_READ_WITH_ALL_BY_SERVICE_CODE = QueryIdentifierBuilder.getInstance().build(Menu.class, "readWithAllByServiceCode");
	Collection<Menu> readWithAllByServiceCode(String serviceCode);
	
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
		public Collection<Menu> readByServiceIdentifier(String serviceIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(Menu.class,QUERY_IDENTIFIER_READ_BY_SERVICE_IDENTIFIER,PARAMETER_NAME_SERVICE_IDENTIFIER,serviceIdentifier);
		}
		
		@Override
		public Long countByServiceIdentifier(String serviceIdentifier) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_SERVICE_IDENTIFIER,PARAMETER_NAME_SERVICE_IDENTIFIER,serviceIdentifier);
		}
		
		@Override
		public Collection<Menu> readByServiceCode(String serviceCode) {
			return QueryExecutor.getInstance().executeReadMany(Menu.class,QUERY_IDENTIFIER_READ_BY_SERVICE_CODE,PARAMETER_NAME_SERVICE_CODE,serviceCode);
		}
		
		@Override
		public Long countByServiceCode(String serviceCode) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_SERVICE_CODE,PARAMETER_NAME_SERVICE_CODE,serviceCode);
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
		public Collection<Menu> readWithAllByServiceIdentifier(String serviceIdentifier) {
			Collection<Menu> menus = readByServiceIdentifier(serviceIdentifier);
			if(CollectionHelper.isEmpty(menus))
				return null;
			__setAll__(menus);
			return menus;
		}
		
		@Override
		public Collection<Menu> readWithAllByServiceCode(String serviceCode) {
			Collection<Menu> menus = readByServiceCode(serviceCode);
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
			if(QUERY_IDENTIFIER_READ_BY_SERVICE_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return readByServiceIdentifier((String)arguments.getFilterFieldValue(PARAMETER_NAME_SERVICE_IDENTIFIER));
			if(QUERY_IDENTIFIER_READ_WITH_ALL_BY_SERVICE_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return readWithAllByServiceIdentifier((String)arguments.getFilterFieldValue(PARAMETER_NAME_SERVICE_IDENTIFIER));
			if(QUERY_IDENTIFIER_READ_BY_SERVICE_CODE.equals(arguments.getQuery().getIdentifier()))
				return readByServiceCode((String)arguments.getFilterFieldValue(PARAMETER_NAME_SERVICE_CODE));
			if(QUERY_IDENTIFIER_READ_WITH_ALL_BY_SERVICE_CODE.equals(arguments.getQuery().getIdentifier()))
				return readWithAllByServiceCode((String)arguments.getFilterFieldValue(PARAMETER_NAME_SERVICE_CODE));
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT.equals(arguments.getQuery().getIdentifier()))
				return count();
			if(QUERY_IDENTIFIER_COUNT_BY_SERVICE_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return countByServiceIdentifier((String)arguments.getFilterFieldValue(PARAMETER_NAME_SERVICE_IDENTIFIER));
			if(QUERY_IDENTIFIER_COUNT_BY_SERVICE_CODE.equals(arguments.getQuery().getIdentifier()))
				return countByServiceCode((String)arguments.getFilterFieldValue(PARAMETER_NAME_SERVICE_CODE));
			return super.count(arguments);
		}
		
		@Override
		protected Class<Menu> getKlass() {
			return Menu.class;
		}
		
		private static void __setAll__(Collection<Menu> menus) {
			if(CollectionHelper.isEmpty(menus))
				return;
			__setProfilesAsString__(menus);
			__setKeycloak__(menus);
		}
		
		private static void __setKeycloak__(Collection<Menu> menus) {
			if(CollectionHelper.isEmpty(menus))
				return;
			Collection<Service> services = menus.stream().filter(menu -> menu.getService() != null).map(menu -> menu.getService()).collect(Collectors.toList());
			if(CollectionHelper.isEmpty(services))
				return;
			Collection<Client> clients = ClientManager.getInstance().getByIdentifiers(FieldHelper.readBusinessIdentifiersAsStrings(services));
			if(CollectionHelper.isEmpty(clients))
				return;
			for(Menu menu : menus) {
				//Client client = null;
				Resource resource = null;
				for(Client clientIndex : clients) {
					if(CollectionHelper.isEmpty(clientIndex.getResources()))
						continue;
					for(Resource resourceIndex : clientIndex.getResources()) {
						if(CollectionHelper.isEmpty(resourceIndex.getUniformResourceIdentifiers()))
							continue;
						if(resourceIndex.getUniformResourceIdentifiers().contains(menu.getUniformResourceIdentifier())) {
							resource = resourceIndex;
							//client = clientIndex;
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
		
		private static void __setProfilesAsString__(Collection<Menu> menus) {
			if(CollectionHelper.isEmpty(menus))
				return;
			Collection<Privilege> menuAsPrivileges = PrivilegeQuerier.getInstance().readBySystemIdentifiers(Privilege.class, FieldHelper.readSystemIdentifiersAsStrings(menus));
			if(CollectionHelper.isEmpty(menuAsPrivileges))
				return;
			Collection<Profile> profiles = ProfileQuerier.getInstance().read();
			if(CollectionHelper.isEmpty(profiles))
				return;
			profiles.forEach(profile -> {
				profile.setPrivileges(PrivilegeQuerier.getInstance().readVisibleByProfilesCodes(List.of(profile.getCode())));
			});
			menus.forEach(menu -> {
				Privilege privilege = CollectionHelper.getFirst(menuAsPrivileges.stream()
						.filter(menuAsPrivilege -> menuAsPrivilege.getIdentifier().equals(menu.getIdentifier())).collect(Collectors.toList()));
				if(privilege != null) {
					menu.setProfilesAsString(profiles.stream()
					.filter(profile -> CollectionHelper.contains(profile.getPrivileges(), privilege))
					.map(profile -> profile.toString())	
					.collect(Collectors.joining(",")));
				}
			});
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