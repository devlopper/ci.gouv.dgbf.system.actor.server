package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;

public interface ProfilePrivilegeQuerier extends Querier {

	String PARAMETER_NAME_PROFILES_CODES = "profilesCodes";
	
	/* read order by profile code ascending by privilege code ascending */
	String QUERY_NAME_READ_ORDER_BY_PROFILE_CODE_ASCENDING_BY_PRIVILEGE_CODE_ASCENDING = "readOrderByProfileCodeAscendingByPrivilegeCodeAscending";
	String QUERY_IDENTIFIER_READ_ORDER_BY_PROFILE_CODE_ASCENDING_BY_PRIVILEGE_CODE_ASCENDING = QueryIdentifierBuilder.getInstance().build(ProfilePrivilege.class, QUERY_NAME_READ_ORDER_BY_PROFILE_CODE_ASCENDING_BY_PRIVILEGE_CODE_ASCENDING);
	String QUERY_VALUE_READ_ORDER_BY_PROFILE_CODE_ASCENDING_BY_PRIVILEGE_CODE_ASCENDING = Language.of(Language.Select.of("t")
			,Language.From.of("ProfilePrivilege t")			
			,Language.Order.of("t.profile.code ASC,t.privilege.code ASC"))
			;
	
	/* read by profiles codes order by privilege code ascending */
	String QUERY_NAME_READ_BY_PROFILES_CODES = "readByProfilesCodes";
	String QUERY_IDENTIFIER_READ_BY_PROFILES_CODES = QueryIdentifierBuilder.getInstance().build(ProfilePrivilege.class, QUERY_NAME_READ_BY_PROFILES_CODES);
	String QUERY_VALUE_READ_BY_PROFILES_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("ProfilePrivilege t")			
			,Language.Where.of("t.profile.code IN :"+PARAMETER_NAME_PROFILES_CODES)			
			,Language.Order.of("t.profile.code ASC,t.privilege.code ASC"))
			;
	Collection<ProfilePrivilege> readByProfilesCodes(Collection<String> profilesCodes);
	
	/* count by profiles codes */
	String QUERY_NAME_COUNT_BY_PROFILES_CODES = "countByProfilesCodes";
	String QUERY_IDENTIFIER_COUNT_BY_PROFILES_CODES = QueryIdentifierBuilder.getInstance().build(ProfilePrivilege.class, QUERY_NAME_COUNT_BY_PROFILES_CODES);
	String QUERY_VALUE_COUNT_BY_PROFILES_CODES = Language.of(Language.Select.of("COUNT(t)")
			,Language.From.of("ProfilePrivilege t")			
			,Language.Where.of("t.profile.code IN :"+PARAMETER_NAME_PROFILES_CODES)			
			);
	Long countByProfilesCodes(Collection<String> profilesCodes);
	
	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements ProfilePrivilegeQuerier,Serializable {
		@Override
		public Collection<ProfilePrivilege> readByProfilesCodes(Collection<String> profilesCodes) {
			return EntityReader.getInstance().readMany(ProfilePrivilege.class, QUERY_IDENTIFIER_READ_BY_PROFILES_CODES, PARAMETER_NAME_PROFILES_CODES,profilesCodes);
		}
		
		@Override
		public Long countByProfilesCodes(Collection<String> profilesCodes) {
			return EntityCounter.getInstance().count(ProfilePrivilege.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_BY_PROFILES_CODES)
					.addFilterField(PARAMETER_NAME_PROFILES_CODES,profilesCodes));
		}
	}
	
	/**/
	
	static ProfilePrivilegeQuerier getInstance() {
		return Helper.getInstance(ProfilePrivilegeQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_ORDER_BY_PROFILE_CODE_ASCENDING_BY_PRIVILEGE_CODE_ASCENDING
				,Query.FIELD_TUPLE_CLASS,ProfilePrivilege.class,Query.FIELD_RESULT_CLASS,ProfilePrivilege.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_ORDER_BY_PROFILE_CODE_ASCENDING_BY_PRIVILEGE_CODE_ASCENDING
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_PROFILES_CODES
				,Query.FIELD_TUPLE_CLASS,ProfilePrivilege.class,Query.FIELD_RESULT_CLASS,ProfilePrivilege.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_PROFILES_CODES
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_PROFILES_CODES
				,Query.FIELD_TUPLE_CLASS,ProfilePrivilege.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_PROFILES_CODES
				)
			);
	}
}