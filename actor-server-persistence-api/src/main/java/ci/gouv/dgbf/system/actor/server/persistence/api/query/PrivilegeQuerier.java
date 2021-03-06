package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.From.from;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.order;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.select;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.annotation.Queries;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;

@Queries(value = {
		@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Privilege.class,name = PrivilegeQuerier.QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING,value = "SELECT t FROM Privilege t ORDER BY t.code ASC")
})
public interface PrivilegeQuerier extends Querier {

	String PARAMETER_NAME_PROFILES_TYPES_CODES = "profilesTypesCodes";
	String PARAMETER_NAME_PROFILES_CODES = "profilesCodes";
	String PARAMETER_NAME_FUNCTIONS_CODES = "functionsCodes";
	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	String PARAMETER_NAME_ACTOR_CODE = "actorCode";
	String PARAMETER_NAME_CHILDREN_CODES = "childrenCodes";
	String PARAMETER_NAME_CHILDREN_IDENTIFIERS = "childrenIdentifiers";
	
	/* read order by code ascending */
	String QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING = "readOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ_ORDER_BY_CODE_ASCENDING = QueryIdentifierBuilder.getInstance().build(Privilege.class, QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING);
	
	/* read by profiles types codes by functions codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().build(Privilege.class, "readByProfilesTypesCodesByFunctionsCodes");
	Collection<Privilege> readByProfilesTypesCodesByFunctionsCodes(Collection<String> typesCodes,Collection<String> functionsCodes);
	
	/* read by actors codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_ACTORS_CODES = QueryIdentifierBuilder.getInstance().build(Privilege.class, "readByActorsCodes");
	Collection<Privilege> readByActorsCodes(Collection<String> actorsCodes);
	
	/* count by actors codes order by code ascending */
	String QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_ACTORS_CODES);
	Long countByActorsCodes(Collection<String> actorsCodes);
	
	/* read by profiles codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_PROFILES_CODES = QueryIdentifierBuilder.getInstance().build(Privilege.class, "readByProfilesCodes");
	Collection<Privilege> readByProfilesCodes(Collection<String> profilesCodes);
	
	/* count by profiles codes order by code ascending */
	String QUERY_IDENTIFIER_COUNT_BY_PROFILES_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_PROFILES_CODES);
	Long countByProfilesCodes(Collection<String> profilesCodes);
	
	/* read by functions codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().build(Privilege.class, "readByFunctionsCodes");
	Collection<Privilege> readByFunctionsCodes(Collection<String> functionsCodes);
	
	/* count by functions codes order by code ascending */
	String QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES);
	Long countByFunctionsCodes(Collection<String> functionsCodes);
	
	/* read by profiles codes not associated order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_PROFILES_CODES_NOT_ASSOCIATED = QueryIdentifierBuilder.getInstance().build(Privilege.class, "readByProfilesCodesNotAssociated");
	Collection<Privilege> readByProfilesCodesNotAssociated(Collection<String> profilesCodes);
	
	/* count by profiles codes not associated */
	String QUERY_IDENTIFIER_COUNT_BY_PROFILES_CODES_NOT_ASSOCIATED = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_PROFILES_CODES_NOT_ASSOCIATED);
	Long countByProfilesCodesNotAssociated(Collection<String> profilesCodes);
	
	/* read parents by children identifiers order by code ascending */
	String QUERY_IDENTIFIER_READ_PARENTS_BY_CHILDREN_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(Privilege.class, "readParentsByChildrenIdentifiers");
	Collection<Privilege> readParentsByChildrenIdentifiers(Collection<String> childrenIdentifiers);
	
	/* count parents by children codes */
	String QUERY_IDENTIFIER_COUNT_PARENTS_BY_CHILDREN_IDENTIFIERS = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_PARENTS_BY_CHILDREN_IDENTIFIERS);
	Long countParentsByChildrenIdentifiers(Collection<String> childrenIdentifiers);
	
	/* read parents by children codes order by code ascending */
	String QUERY_IDENTIFIER_READ_PARENTS_BY_CHILDREN_CODES = QueryIdentifierBuilder.getInstance().build(Privilege.class, "readParentsByChildrenCodes");
	Collection<Privilege> readParentsByChildrenCodes(Collection<String> childrenCodes);
	
	/* count parents by children codes */
	String QUERY_IDENTIFIER_COUNT_PARENTS_BY_CHILDREN_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_PARENTS_BY_CHILDREN_CODES);
	Long countParentsByChildrenCodes(Collection<String> childrenCodes);
	
	/* read visible by actor code order by code ascending */
	String QUERY_IDENTIFIER_READ_VISIBLE_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().build(Privilege.class, "readVisibleByActorCode");
	Collection<Privilege> readVisibleByActorCode(String actorCode);
	
	/* count visible by actor code */
	String QUERY_IDENTIFIER_COUNT_VISIBLE_SECTIONS_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_VISIBLE_BY_ACTOR_CODE);
	Long countVisibleByActorCode(String actorCode);
	
	/* read visible by profiles codes order by code ascending */
	String QUERY_IDENTIFIER_READ_VISIBLE_BY_PROFILES_CODES = QueryIdentifierBuilder.getInstance().build(Privilege.class, "readVisibleByProfilesCodes");
	Collection<Privilege> readVisibleByProfilesCodes(Collection<String> profilesCodes);
	
	/* count visible by actor code */
	String QUERY_IDENTIFIER_COUNT_VISIBLE_BY_PROFILES_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_VISIBLE_BY_PROFILES_CODES);
	Long countVisibleByProfilesCodes(Collection<String> profilesCodes);
	
	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements PrivilegeQuerier,Serializable {
		@Override
		public Collection<Privilege> readByProfilesTypesCodesByFunctionsCodes(Collection<String> profilesTypesCodes,Collection<String> functionsCodes) {
			return QueryExecutor.getInstance().executeReadMany(Privilege.class, QUERY_IDENTIFIER_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES
					, PARAMETER_NAME_PROFILES_TYPES_CODES,profilesTypesCodes,PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes);
		}
		
		@Override
		public Collection<Privilege> readByActorsCodes(Collection<String> actorsCodes) {
			return QueryExecutor.getInstance().executeReadMany(Privilege.class, QUERY_IDENTIFIER_READ_BY_ACTORS_CODES, PARAMETER_NAME_ACTORS_CODES,actorsCodes);
		}
		
		@Override
		public Long countByActorsCodes(Collection<String> actorsCodes) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES, PARAMETER_NAME_ACTORS_CODES,actorsCodes);
		}
		
		@Override
		public Collection<Privilege> readByProfilesCodes(Collection<String> profilesCodes) {
			return QueryExecutor.getInstance().executeReadMany(Privilege.class, QUERY_IDENTIFIER_READ_BY_PROFILES_CODES, PARAMETER_NAME_PROFILES_CODES,profilesCodes);
		}
		
		@Override
		public Long countByProfilesCodes(Collection<String> profilesCodes) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_PROFILES_CODES, PARAMETER_NAME_PROFILES_CODES,profilesCodes);
		}
		
		@Override
		public Collection<Privilege> readByFunctionsCodes(Collection<String> functionsCodes) {
			return QueryExecutor.getInstance().executeReadMany(Privilege.class, QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES, PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes);
		}
		
		@Override
		public Long countByFunctionsCodes(Collection<String> functionsCodes) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_PROFILES_CODES, PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes);
		}
		
		@Override
		public Collection<Privilege> readByProfilesCodesNotAssociated(Collection<String> profilesCodes) {
			return QueryExecutor.getInstance().executeReadMany(Privilege.class, QUERY_IDENTIFIER_READ_BY_PROFILES_CODES_NOT_ASSOCIATED, PARAMETER_NAME_PROFILES_CODES,profilesCodes);
		}
		
		@Override
		public Long countByProfilesCodesNotAssociated(Collection<String> profilesCodes) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_PROFILES_CODES_NOT_ASSOCIATED, PARAMETER_NAME_PROFILES_CODES,profilesCodes);
		}
		
		@Override
		public Collection<Privilege> readParentsByChildrenCodes(Collection<String> childrenCodes) {
			return QueryExecutor.getInstance().executeReadMany(Privilege.class, QUERY_IDENTIFIER_READ_PARENTS_BY_CHILDREN_CODES, PARAMETER_NAME_CHILDREN_CODES,childrenCodes);
		}
		
		@Override
		public Long countParentsByChildrenCodes(Collection<String> childrenCodes) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_PARENTS_BY_CHILDREN_CODES, PARAMETER_NAME_CHILDREN_CODES,childrenCodes);
		}
		
		@Override
		public Collection<Privilege> readParentsByChildrenIdentifiers(Collection<String> childrenIdentifiers) {
			return QueryExecutor.getInstance().executeReadMany(Privilege.class, QUERY_IDENTIFIER_READ_PARENTS_BY_CHILDREN_CODES, PARAMETER_NAME_CHILDREN_CODES,childrenIdentifiers);
		}
		
		@Override
		public Long countParentsByChildrenIdentifiers(Collection<String> childrenIdentifiers) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_PARENTS_BY_CHILDREN_CODES, PARAMETER_NAME_CHILDREN_CODES,childrenIdentifiers);
		}
		
		@Override
		public Collection<Privilege> readVisibleByProfilesCodes(Collection<String> profilesCodes) {
			if(CollectionHelper.isEmpty(profilesCodes))
				return null;
			Collection<Privilege> children = readByProfilesCodes(profilesCodes);
			if(CollectionHelper.isEmpty(children))
				return null;			
			Collection<Privilege> privileges = new HashSet<>();
			privileges.addAll(children);			
			Collection<Privilege> parents = readParentsByChildrenCodes(children.stream().map(child -> child.getCode()).collect(Collectors.toSet()));
			if(CollectionHelper.isNotEmpty(parents))
				privileges.addAll(parents);			
			return privileges;
		}
		
		@Override
		public Long countVisibleByProfilesCodes(Collection<String> profilesCodes) {
			return NumberHelper.getLong(CollectionHelper.getSize(readVisibleByProfilesCodes(profilesCodes)));
		}
		
		@Override
		public Collection<Privilege> readVisibleByActorCode(String actorCode) {
			Collection<Privilege> privileges = null;
			Collection<Privilege> children = StringHelper.isBlank(actorCode) ? null : readByActorsCodes(List.of(actorCode));
			if(CollectionHelper.isNotEmpty(children)) {
				if(privileges == null)
					privileges = new HashSet<>();
				privileges.addAll(children);
			}
			Collection<Privilege> parents = CollectionHelper.isEmpty(children) ? null : readParentsByChildrenCodes(children.stream().map(child -> child.getCode()).collect(Collectors.toSet()));
			if(CollectionHelper.isNotEmpty(parents)) {
				if(privileges == null)
					privileges = new HashSet<>();
				privileges.addAll(parents);
			}
			return privileges;
		}
		
		@Override
		public Long countVisibleByActorCode(String actorCode) {
			return NumberHelper.getLong(CollectionHelper.getSize(readVisibleByActorCode(actorCode)));
		}
	}
	
	/**/
	
	static PrivilegeQuerier getInstance() {
		return Helper.getInstance(PrivilegeQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Privilege.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("t")
						,Language.From.of("Privilege t"
								+ " JOIN ProfilePrivilege profilePrivilege ON profilePrivilege.privilege.identifier = t.identifier"
								+ " JOIN ProfileFunction profileFunction ON profileFunction.profile.identifier = profilePrivilege.profile.identifier"
								)			
						,Language.Where.of("profileFunction.profile.type.code IN :"+PARAMETER_NAME_PROFILES_TYPES_CODES+" AND profileFunction.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)			
						,Language.Order.of("t.code ASC"))
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACTORS_CODES
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Privilege.class
				,Query.FIELD_VALUE,jpql(select("DISTINCT(t.identifier),t.code,t.name,t.type,t.parentIdentifier"),getReadByActorsFromWhere(),order("t.code ASC"))
				).setTupleFieldsNamesIndexesFromFieldsNames(Privilege.FIELD_IDENTIFIER,Privilege.FIELD_CODE,Privilege.FIELD_NAME,Privilege.FIELD_TYPE,Privilege.FIELD_PARENT_IDENTIFIER)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,jpql(select("COUNT(DISTINCT t.identifier)"),getReadByActorsFromWhere()))
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_PROFILES_CODES
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Privilege.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("t")
						,Language.From.of("Privilege t"
								+ " JOIN ProfilePrivilege profilePrivilege ON profilePrivilege.privilege.identifier = t.identifier")			
						,Language.Where.of("profilePrivilege.profile.code IN :"+PARAMETER_NAME_PROFILES_CODES)			
						,Language.Order.of("t.code ASC"))
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_PROFILES_CODES
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("COUNT(t.identifier)")
						,Language.From.of("Privilege t"
								+ " JOIN ProfilePrivilege profilePrivilege ON profilePrivilege.privilege.identifier = t.identifier")			
						,Language.Where.of("profilePrivilege.profile.code IN :"+PARAMETER_NAME_PROFILES_CODES)			
						)
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Privilege.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("t")
						,Language.From.of("Privilege t"
								+ " JOIN ProfilePrivilege profilePrivilege ON profilePrivilege.privilege.identifier = t.identifier"
								+ " JOIN ProfileFunction profileFunction ON profileFunction.profile.identifier = profilePrivilege.profile.identifier")			
						,Language.Where.of("profileFunction.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)			
						,Language.Order.of("t.code ASC"))
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_FUNCTIONS_CODES
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("COUNT(t.identifier)")
						,Language.From.of("Privilege t"
								+ " JOIN ProfilePrivilege profilePrivilege ON profilePrivilege.privilege.identifier = t.identifier"
								+ " JOIN ProfileFunction profileFunction ON profileFunction.profile.identifier = profilePrivilege.profile.identifier")			
						,Language.Where.of("profileFunction.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)			
						)
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_PROFILES_CODES_NOT_ASSOCIATED
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Privilege.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("t")
						,Language.From.of("Privilege t")			
						,Language.Where.of("NOT EXISTS(SELECT pp FROM ProfilePrivilege pp WHERE pp.privilege.identifier = t.identifier AND pp.profile.code IN :"+PARAMETER_NAME_PROFILES_CODES+")")			
						,Language.Order.of("t.code ASC"))
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_PROFILES_CODES_NOT_ASSOCIATED
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("COUNT(t.identifier)")
						,Language.From.of("Privilege t")			
						,Language.Where.of("NOT EXISTS(SELECT pp FROM ProfilePrivilege pp WHERE pp.privilege.identifier = t.identifier AND pp.profile.code IN :"+PARAMETER_NAME_PROFILES_CODES+")")			
						)
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_PARENTS_BY_CHILDREN_CODES
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Privilege.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("DISTINCT(t.identifier),t.code,t.name,t.type,t.parentIdentifier")
						,Language.From.of("Privilege t"
								,Language.From.leftJoin("Privilege", "l1", Privilege.FIELD_PARENT_IDENTIFIER, "t", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l2", Privilege.FIELD_PARENT_IDENTIFIER, "l1", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l3", Privilege.FIELD_PARENT_IDENTIFIER, "l2", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l4", Privilege.FIELD_PARENT_IDENTIFIER, "l3", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l5", Privilege.FIELD_PARENT_IDENTIFIER, "l4", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l6", Privilege.FIELD_PARENT_IDENTIFIER, "l5", Privilege.FIELD_IDENTIFIER)
								)
						,Language.Where.of(Language.Where.or("l1.code IN :"+PARAMETER_NAME_CHILDREN_CODES,"l2.code IN :"+PARAMETER_NAME_CHILDREN_CODES
								,"l3.code IN :"+PARAMETER_NAME_CHILDREN_CODES,"l4.code IN :"+PARAMETER_NAME_CHILDREN_CODES,"l5.code IN :"+PARAMETER_NAME_CHILDREN_CODES
								,"l6.code IN :"+PARAMETER_NAME_CHILDREN_CODES))
						,Language.Order.of("t.code ASC"))
				).setTupleFieldsNamesIndexes(MapHelper.instantiateStringIntegerByStrings(Privilege.FIELD_IDENTIFIER,Privilege.FIELD_CODE
						,Privilege.FIELD_NAME,Privilege.FIELD_TYPE,Privilege.FIELD_PARENT_IDENTIFIER))
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_PARENTS_BY_CHILDREN_CODES
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("COUNT(t.identifier)")
						,Language.From.of("Privilege t"
								,Language.From.leftJoin("Privilege", "l1", Privilege.FIELD_PARENT_IDENTIFIER, "t", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l2", Privilege.FIELD_PARENT_IDENTIFIER, "l1", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l3", Privilege.FIELD_PARENT_IDENTIFIER, "l2", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l4", Privilege.FIELD_PARENT_IDENTIFIER, "l3", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l5", Privilege.FIELD_PARENT_IDENTIFIER, "l4", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l6", Privilege.FIELD_PARENT_IDENTIFIER, "l5", Privilege.FIELD_IDENTIFIER)
								)
						,Language.Where.of(Language.Where.or("l1.code IN :"+PARAMETER_NAME_CHILDREN_CODES,"l2.code IN :"+PARAMETER_NAME_CHILDREN_CODES
								,"l3.code IN :"+PARAMETER_NAME_CHILDREN_CODES,"l4.code IN :"+PARAMETER_NAME_CHILDREN_CODES,"l5.code IN :"+PARAMETER_NAME_CHILDREN_CODES
								,"l6.code IN :"+PARAMETER_NAME_CHILDREN_CODES))
						)
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_PARENTS_BY_CHILDREN_IDENTIFIERS
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Privilege.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("DISTINCT(t.identifier),t.code,t.name,t.type,t.parentIdentifier")
						,Language.From.of("Privilege t"
								,Language.From.leftJoin("Privilege", "l1", Privilege.FIELD_PARENT_IDENTIFIER, "t", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l2", Privilege.FIELD_PARENT_IDENTIFIER, "l1", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l3", Privilege.FIELD_PARENT_IDENTIFIER, "l2", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l4", Privilege.FIELD_PARENT_IDENTIFIER, "l3", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l5", Privilege.FIELD_PARENT_IDENTIFIER, "l4", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l6", Privilege.FIELD_PARENT_IDENTIFIER, "l5", Privilege.FIELD_IDENTIFIER)
								)
						,Language.Where.of(Language.Where.or("l1.identifier IN :"+PARAMETER_NAME_CHILDREN_IDENTIFIERS,"l2.identifier IN :"+PARAMETER_NAME_CHILDREN_IDENTIFIERS
								,"l3.identifier IN :"+PARAMETER_NAME_CHILDREN_IDENTIFIERS,"l4.identifier IN :"+PARAMETER_NAME_CHILDREN_IDENTIFIERS,"l5.identifier IN :"+PARAMETER_NAME_CHILDREN_IDENTIFIERS
								,"l6.identifier IN :"+PARAMETER_NAME_CHILDREN_IDENTIFIERS))
						,Language.Order.of("t.identifier ASC"))
				).setTupleFieldsNamesIndexes(MapHelper.instantiateStringIntegerByStrings(Privilege.FIELD_IDENTIFIER,Privilege.FIELD_CODE
						,Privilege.FIELD_NAME,Privilege.FIELD_TYPE,Privilege.FIELD_PARENT_IDENTIFIER))
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_PARENTS_BY_CHILDREN_IDENTIFIERS
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("COUNT(t.identifier)")
						,Language.From.of("Privilege t"
								,Language.From.leftJoin("Privilege", "l1", Privilege.FIELD_PARENT_IDENTIFIER, "t", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l2", Privilege.FIELD_PARENT_IDENTIFIER, "l1", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l3", Privilege.FIELD_PARENT_IDENTIFIER, "l2", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l4", Privilege.FIELD_PARENT_IDENTIFIER, "l3", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l5", Privilege.FIELD_PARENT_IDENTIFIER, "l4", Privilege.FIELD_IDENTIFIER)
								,Language.From.leftJoin("Privilege", "l6", Privilege.FIELD_PARENT_IDENTIFIER, "l5", Privilege.FIELD_IDENTIFIER)
								)
						,Language.Where.of(Language.Where.or("l1.identifier IN :"+PARAMETER_NAME_CHILDREN_IDENTIFIERS,"l2.identifier IN :"+PARAMETER_NAME_CHILDREN_IDENTIFIERS
								,"l3.identifier IN :"+PARAMETER_NAME_CHILDREN_IDENTIFIERS,"l4.identifier IN :"+PARAMETER_NAME_CHILDREN_IDENTIFIERS,"l5.identifier IN :"+PARAMETER_NAME_CHILDREN_IDENTIFIERS
								,"l6.identifier IN :"+PARAMETER_NAME_CHILDREN_IDENTIFIERS))
						)
				)
			);
	}
	
	static String getReadByActorsFromWhere() {
		return jpql(
				from("Privilege t","JOIN ProfilePrivilege profilePrivilege ON profilePrivilege.privilege.identifier = t.identifier"
					,"JOIN ActorProfile actorProfile ON actorProfile.profile.identifier = profilePrivilege.profile.identifier")			
				,where("actorProfile.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES)			
			);
	}
}