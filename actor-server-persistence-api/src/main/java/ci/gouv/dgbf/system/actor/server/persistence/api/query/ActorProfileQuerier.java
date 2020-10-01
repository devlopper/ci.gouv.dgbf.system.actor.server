package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;

public interface ActorProfileQuerier extends Querier {

	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	String PARAMETER_NAME_ACTOR_CODE = "actorCode";
	String PARAMETER_NAME_PROFILES_CODES = "profilesCodes";
	
	/* read by actors codes order by scope code ascending */
	String QUERY_NAME_READ_BY_ACTORS_CODES = "readByActorsCodes";
	String QUERY_IDENTIFIER_READ_BY_ACTORS_CODES = QueryIdentifierBuilder.getInstance().build(ActorProfile.class, QUERY_NAME_READ_BY_ACTORS_CODES);
	String QUERY_VALUE_READ_BY_ACTORS_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("ActorProfile t")			
			,Language.Where.of("t.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES)			
			,Language.Order.of("t.profile.code ASC"))
			;
	Collection<ActorProfile> readByActorsCodes(Collection<String> actorsCodes);
	
	/* read by profiles codes */
	String QUERY_IDENTIFIER_READ_BY_PROFILES_CODES = QueryIdentifierBuilder.getInstance().build(ActorProfile.class, "readByProfilesCodes");
	Collection<ActorProfile> readByProfilesCodes(Collection<String> profilesCodes);
	default Collection<ActorProfile> readByProfilesCodes(String...profilesCodes) {
		if(ArrayHelper.isEmpty(profilesCodes))
			return null;
		return readByProfilesCodes(CollectionHelper.listOf(profilesCodes));
	}
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements ActorProfileQuerier,Serializable {
		@Override
		public Collection<ActorProfile> readByActorsCodes(Collection<String> actorsCodes) {
			return EntityReader.getInstance().readMany(ActorProfile.class, QUERY_IDENTIFIER_READ_BY_ACTORS_CODES, PARAMETER_NAME_ACTORS_CODES,actorsCodes);
		}
		
		@Override
		public Collection<ActorProfile> readByProfilesCodes(Collection<String> profilesCodes) {
			return QueryExecutor.getInstance().executeReadMany(ActorProfile.class, QUERY_IDENTIFIER_READ_BY_PROFILES_CODES,PARAMETER_NAME_PROFILES_CODES, profilesCodes);
		}
	}
	
	/**/
	
	static ActorProfileQuerier getInstance() {
		return Helper.getInstance(ActorProfileQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(
				Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACTORS_CODES
				,Query.FIELD_TUPLE_CLASS,ActorProfile.class,Query.FIELD_RESULT_CLASS,ActorProfile.class,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ACTORS_CODES)
				,Query.buildSelect(ActorProfile.class, QUERY_IDENTIFIER_READ_BY_PROFILES_CODES, "SELECT t FROM ActorProfile t WHERE t.profile.code IN :"
				+PARAMETER_NAME_PROFILES_CODES)
			);
	}
}