package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.persistence.query.Language.jpql;
import static org.cyk.utility.persistence.query.Language.From.from;
import static org.cyk.utility.persistence.query.Language.Order.order;
import static org.cyk.utility.persistence.query.Language.Select.select;
import static org.cyk.utility.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.annotation.Queries;
import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.query.Language.Where;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutor;
import org.cyk.utility.persistence.server.query.executor.DynamicOneExecutor;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Menu;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Service;

@Queries(value = {
		@org.cyk.utility.persistence.annotation.Query(tupleClass = Profile.class,name = ProfileQuerier.QUERY_NAME_READ
				,value = "SELECT t FROM Profile t ORDER BY t.code ASC")
})
public interface ProfileQuerier extends Querier.CodableAndNamable<Profile> {

	String PARAMETER_NAME_TYPES_CODES = "typesCodes";
	String PARAMETER_NAME_FUNCTIONS_CODES = "functionsCodes";
	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	String PARAMETER_NAME_SERVICES_IDENTIFIERS = "servicesIdentifiers";
	
	String QUERY_IDENTIFIER_READ_DYNAMIC = QueryIdentifierBuilder.getInstance().build(Profile.class, QueryName.READ_DYNAMIC);	
	String QUERY_IDENTIFIER_READ_DYNAMIC_ONE = QueryIdentifierBuilder.getInstance().build(Profile.class, QueryName.READ_DYNAMIC_ONE);
	String QUERY_IDENTIFIER_COUNT_DYNAMIC = QueryIdentifierBuilder.getInstance().build(Profile.class, QueryName.COUNT_DYNAMIC);
	
	/* read order by code ascending */
	String QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_SYSTEME = QueryIdentifierBuilder.getInstance().build(Profile.class, "readWhereTypeIsSystemeOrderByCodeAscending");
	Collection<Profile> readWhereTypeIsSystemeOrderByCodeAscending();
	
	String QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_SYSTEME = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_SYSTEME);
	Long countWhereTypeIsSystemeOrderByCodeAscending();
	
	/* read order by code ascending */
	String QUERY_NAME_READ = "readOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(Profile.class, QUERY_NAME_READ);
	Collection<Profile> read();
	
	/* read by types codes by functions codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_TYPES_CODES_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().build(Profile.class, "readByTypesCodesByFunctionsCodes");
	Collection<Profile> readByTypesCodesByFunctionsCodes(Collection<String> typesCodes,Collection<String> functionsCodes);
	
	/* read by types codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Profile.class, "readByTypesCodes");
	Collection<Profile> readByTypesCodes(Collection<String> typesCodes);
	
	/* count by types codes */
	String QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_TYPES_CODES);
	Long countByTypesCodes(Collection<String> typesCodes);
	
	/* read by actors codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_ACTORS_CODES = QueryIdentifierBuilder.getInstance().build(Profile.class, "readByActorsCodes");
	Collection<Profile> readByActorsCodes(Collection<String> actorsCodes);
	
	/* read by functions codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().build(Profile.class, "readByFunctionsCodes");
	Collection<Profile> readByFunctionsCodes(Collection<String> functionsCodes);
	
	/* read by services identifiers order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_SERVICES_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(Service.class, "readByServicesIdentifiers");
	Collection<Profile> readByServicesIdentifiers(Collection<String> servicesIdentifiers);
	
	Collection<Profile> readByMenus(Collection<Menu> menus);
	Collection<Profile> readByMenus(Menu...menus);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Profile> implements ProfileQuerier,Serializable {
		@SuppressWarnings("unchecked")
		@Override
		public Collection<Profile> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().read(Profile.class,arguments.setQuery(null));
			if(QUERY_IDENTIFIER_READ.equals(arguments.getQuery().getIdentifier()))
				return read();
			if(QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_SYSTEME.equals(arguments.getQuery().getIdentifier()))
				return readWhereTypeIsSystemeOrderByCodeAscending();
			if(QUERY_IDENTIFIER_READ_BY_TYPES_CODES.equals(arguments.getQuery().getIdentifier()))
				return readByTypesCodes((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_TYPES_CODES));
			if(QUERY_IDENTIFIER_READ_BY_ACTORS_CODES.equals(arguments.getQuery().getIdentifier()))
				return readByActorsCodes((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_ACTORS_CODES));
			if(QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES.equals(arguments.getQuery().getIdentifier()))
				return readByFunctionsCodes((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_FUNCTIONS_CODES));
			if(QUERY_IDENTIFIER_READ_BY_TYPES_CODES_BY_FUNCTIONS_CODES.equals(arguments.getQuery().getIdentifier()))
				return readByTypesCodesByFunctionsCodes((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_TYPES_CODES)
						,(Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_FUNCTIONS_CODES));
			if(QUERY_IDENTIFIER_READ_BY_SERVICES_IDENTIFIERS.equals(arguments.getQuery().getIdentifier()))
				return readByServicesIdentifiers((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_SERVICES_IDENTIFIERS));
			return super.readMany(arguments);
		}
		
		@Override
		public Profile readOne(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC_ONE.equals(arguments.getQuery().getIdentifier()))
				return DynamicOneExecutor.getInstance().read(Profile.class,arguments.setQuery(null));
			return super.readOne(arguments);
		}
		
		@SuppressWarnings("unchecked")
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().count(Profile.class,arguments.setQuery(null));
			if(QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_SYSTEME.equals(arguments.getQuery().getIdentifier()))
				return countWhereTypeIsSystemeOrderByCodeAscending();	
			if(QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES.equals(arguments.getQuery().getIdentifier()))
				return countByTypesCodes((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_TYPES_CODES));	
			return super.count(arguments);
		}
		
		@Override
		public Collection<Profile> readWhereTypeIsSystemeOrderByCodeAscending() {
			return QueryExecutor.getInstance().executeReadMany(Profile.class, QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_SYSTEME);
		}
		
		@Override
		public Long countWhereTypeIsSystemeOrderByCodeAscending() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_SYSTEME);
		}
		
		@Override
		public Collection<Profile> read() {
			return QueryExecutor.getInstance().executeReadMany(Profile.class, QUERY_IDENTIFIER_READ);
		}
		
		@Override
		public Collection<Profile> readByTypesCodesByFunctionsCodes(Collection<String> typesCodes,Collection<String> functionsCodes) {
			return QueryExecutor.getInstance().executeReadMany(Profile.class, QUERY_IDENTIFIER_READ_BY_TYPES_CODES_BY_FUNCTIONS_CODES
					, PARAMETER_NAME_TYPES_CODES,typesCodes,PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes);
		}
		
		@Override
		public Collection<Profile> readByTypesCodes(Collection<String> typesCodes) {
			return QueryExecutor.getInstance().executeReadMany(Profile.class, QUERY_IDENTIFIER_READ_BY_TYPES_CODES, PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Long countByTypesCodes(Collection<String> typesCodes) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES,PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Collection<Profile> readByActorsCodes(Collection<String> actorsCodes) {
			return QueryExecutor.getInstance().executeReadMany(Profile.class, QUERY_IDENTIFIER_READ_BY_ACTORS_CODES, PARAMETER_NAME_ACTORS_CODES,actorsCodes);
		}
		
		@Override
		public Collection<Profile> readByFunctionsCodes(Collection<String> functionsCodes) {
			return QueryExecutor.getInstance().executeReadMany(Profile.class, QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES, PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes);
		}
		
		@Override
		public Collection<Profile> readByServicesIdentifiers(Collection<String> servicesIdentifiers) {
			if(CollectionHelper.isEmpty(servicesIdentifiers))
				return null;
			//services are privileges
			Collection<Privilege> servicesAsPrivileges = PrivilegeQuerier.getInstance().readBySystemIdentifiers(Privilege.class, servicesIdentifiers);
			if(CollectionHelper.isEmpty(servicesAsPrivileges))
				return null;
			Collection<Profile> profiles = ProfileQuerier.getInstance().read();
			if(CollectionHelper.isEmpty(profiles))
				return null;
			Collection<Profile> result = null;
			for(Profile profile : profiles) {
				Collection<Privilege> visibles = PrivilegeQuerier.getInstance().readVisibleByProfilesCodes(List.of(profile.getCode()));
				if(CollectionHelper.isEmpty(visibles))
					continue;
				if(visibles.stream().filter(servicesAsPrivileges::contains).collect(Collectors.toList()).isEmpty())
					continue;
				if(result == null)
					result = new ArrayList<>();
				result.add(profile);
			}
			return result;
		}
		
		@Override
		public Collection<Profile> readByMenus(Collection<Menu> menus) {
			if(CollectionHelper.isEmpty(menus))
				return null;
			Collection<Privilege> menuAsPrivileges = PrivilegeQuerier.getInstance().readBySystemIdentifiers(Privilege.class, FieldHelper.readSystemIdentifiersAsStrings(menus));
			if(CollectionHelper.isEmpty(menuAsPrivileges))
				return null;
			Collection<Profile> profiles = ProfileQuerier.getInstance().read();
			if(CollectionHelper.isEmpty(profiles))
				return null;
			profiles.forEach(profile -> {
				profile.setPrivileges(PrivilegeQuerier.getInstance().readVisibleByProfilesCodes(List.of(profile.getCode())));
			});
			Collection<Profile> menusProfiles = null;
			for(Menu menu : menus) {
				Privilege privilege = CollectionHelper.getFirst(menuAsPrivileges.stream()
						.filter(menuAsPrivilege -> menuAsPrivilege.getIdentifier().equals(menu.getIdentifier())).collect(Collectors.toList()));
				if(privilege == null)
					continue;
				if(menusProfiles == null)
					menusProfiles = new LinkedHashSet<>();
				menusProfiles.addAll(profiles.stream().filter(profile -> CollectionHelper.contains(profile.getPrivileges(), privilege)).collect(Collectors.toList()));				
			}
			return menusProfiles;
		}
		
		@Override
		public Collection<Profile> readByMenus(Menu... menus) {
			if(ArrayHelper.isEmpty(menus))
				return null;
			return readByMenus(CollectionHelper.listOf(menus));
		}
		
		@Override
		protected Class<Profile> getKlass() {
			return Profile.class;
		}
	}
	
	/**/
	
	static ProfileQuerier getInstance() {
		return Helper.getInstance(ProfileQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(Profile.class);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_SYSTEME
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Profile.class
				,Query.FIELD_VALUE,"SELECT t FROM Profile t WHERE t.type.code = '"+ProfileType.CODE_SYSTEME+"' ORDER BY t.code ASC")
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_SYSTEME
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,"SELECT COUNT(t) FROM Profile t WHERE t.type.code = '"+ProfileType.CODE_SYSTEME+"'")
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_TYPES_CODES_BY_FUNCTIONS_CODES
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Profile.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("t")
						,Language.From.of("Profile t JOIN ProfileFunction profileFunction ON profileFunction.profile.identifier = t.identifier")			
						,Language.Where.of("profileFunction.profile.type.code IN :"+PARAMETER_NAME_TYPES_CODES+" AND profileFunction.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)			
						,Language.Order.of("t.code ASC"))
				)
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Profile.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("t")
						,Language.From.of("Profile t")			
						,Language.Where.of("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES)
						,Language.Order.of("t.code ASC"))
				)
			);
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("COUNT(t.identifier)")
						,Language.From.of("Profile t")			
						,Language.Where.of("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES))
				)
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACTORS_CODES
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Profile.class
				,Query.FIELD_VALUE,jpql(select("t")
						,from("Profile t JOIN ActorProfile actorProfile ON actorProfile.profile = t")			
						,where(Where.in("actorProfile.actor", Actor.FIELD_CODE, PARAMETER_NAME_ACTORS_CODES))			
						,order("t.code ASC"))
				)
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Profile.class
				,Query.FIELD_VALUE,jpql(select("t")
						,from("Profile t JOIN ProfileFunction profileFunction ON profileFunction.profile.identifier = t.identifier")			
						,where("profileFunction.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)			
						,order("t.code ASC"))
				)
			);
	}
}