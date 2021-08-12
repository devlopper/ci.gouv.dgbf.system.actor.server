package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.Helper;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScopeRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public class ActorScopeRequestActorAsStringScopeTypeAsStringScopeAsStringGrantedAndGrantedAsStringReader extends AbstractActorScopeRequestReaderImpl implements Serializable {

	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",ActorScopeRequest.FIELD_IDENTIFIER,ActorScopeRequest.FIELD_GRANTED);
		arguments.getProjection(Boolean.TRUE).addFromTuple("a.identity", Identity.FIELD_FIRST_NAME,Identity.FIELD_LAST_NAMES,Identity.FIELD_ELECTRONIC_MAIL_ADDRESS);
		arguments.getProjection(Boolean.TRUE).add(Language.Select.concatCodeName("s"));
		arguments.getProjection(Boolean.TRUE).addFromTuple("st", ScopeType.FIELD_NAME);
		arguments.getTuple(Boolean.TRUE).add("ActorScopeRequest t").addJoins("JOIN Actor a ON a = t.actor","JOIN Scope s ON s = t.scope","JOIN ScopeType st ON st = s.type");
		arguments.getPredicate(Boolean.TRUE).add("t.identifier IN :"+Querier.PARAMETER_NAME_IDENTIFIERS);
		return QueryStringBuilder.getInstance().build(arguments);
	}
	
	@Override
	protected void __set__(ActorScopeRequest actorScopeRequest, Object[] array) {
		Integer index = 1;
		//Identity Join
		actorScopeRequest.setGranted((Boolean) array[index++]);
		if(actorScopeRequest.getGranted() == null)
			actorScopeRequest.setGrantedAsString("En cours de traitement");
		else
			actorScopeRequest.setGrantedAsString(Helper.ifTrueYesElseNo(actorScopeRequest.getGranted()));
		actorScopeRequest.setActorAsString(String.format("%s %s(%s)", getAsString(array, index++),getAsString(array, index++),getAsString(array, index++)));
		actorScopeRequest.setScopeAsString(getAsString(array, index++));
		actorScopeRequest.setScopeTypeAsString(getAsString(array, index++));
	}
}