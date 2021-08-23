package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import java.util.Collection;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorProfileRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfileRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public class ActorsPersistenceImplUnitTestDev extends AbstractUnitTestLive {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Override
	protected void __listenBefore__() {
		super.__listenBefore__();
		EXCAT = Boolean.FALSE;
	}

	@Test
	public void read(){
		QueryExecutorArguments arguments = new QueryExecutorArguments();
		arguments.setQuery(new Query().setIdentifier(ActorQuerier.QUERY_IDENTIFIER_READ_DYNAMIC));
		arguments.addFilterFieldsValues(ActorQuerier.PARAMETER_NAME_SEARCH,"christian",ActorQuerier.PARAMETER_NAME_VISIBLE_SCOPE_TYPE_CODE,ScopeType.CODE_SECTION
				,ActorQuerier.PARAMETER_NAME_VISIBLE_SCOPE_IDENTIFIER,"SECTION01172e2c-7eb0-41a3-8d18-4c786e933ff7");
		Collection<Actor> actors = EntityReader.getInstance().readMany(Actor.class, arguments);
		System.out.println(actors);
	}
	
	@Test
	public void readActorProfileRequest(){
		QueryExecutorArguments arguments = new QueryExecutorArguments();
		arguments.setQuery(new Query().setIdentifier(ActorProfileRequestQuerier.QUERY_IDENTIFIER_READ_DYNAMIC));
		arguments.addProcessableTransientFieldsNames(ActorProfileRequest.FIELDS_ACTOR_AS_STRING_PROFILE_TYPE_AS_STRING_PROFILE_AS_STRING_GRANTED_AND_GRANTED_AS_STRING);
		Collection<ActorProfileRequest> collection = EntityReader.getInstance().readMany(ActorProfileRequest.class, arguments);
		if(CollectionHelper.isNotEmpty(collection))
			collection.forEach(actorProfileRequest -> {
				System.out.println(actorProfileRequest.getActorAsString()+" "+actorProfileRequest.getProfileTypeAsString()+" "+actorProfileRequest.getProfileAsString()+" "+actorProfileRequest.getGrantedAsString());
			});
	}
}