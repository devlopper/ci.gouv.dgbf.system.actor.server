package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.Helper;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfileRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;

public class ActorProfileRequestActorAsStringProfileAsStringGrantedAndGrantedAsStringReader extends AbstractActorProfileRequestReaderImpl implements Serializable {

	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",ActorProfileRequest.FIELD_IDENTIFIER,ActorProfileRequest.FIELD_GRANTED);
		arguments.getProjection(Boolean.TRUE).addFromTuple("a.identity", Identity.FIELD_FIRST_NAME,Identity.FIELD_LAST_NAMES,Identity.FIELD_ELECTRONIC_MAIL_ADDRESS);
		arguments.getProjection(Boolean.TRUE).add(Language.Select.concatCodeName("s"));
		arguments.getTuple(Boolean.TRUE).add("ActorProfileRequest t").addJoins("JOIN Actor a ON a = t.actor","JOIN Profile p ON p = t.profile");
		arguments.getPredicate(Boolean.TRUE).add("t.identifier IN :"+Querier.PARAMETER_NAME_IDENTIFIERS);
		return QueryStringBuilder.getInstance().build(arguments);
	}
	
	@Override
	protected void __set__(ActorProfileRequest actorProfileRequest, Object[] array) {
		Integer index = 1;
		//Identity Join
		actorProfileRequest.setGranted((Boolean) array[index++]);
		if(actorProfileRequest.getGranted() == null)
			actorProfileRequest.setGrantedAsString("En cours de traitement");
		else
			actorProfileRequest.setGrantedAsString(Helper.ifTrueYesElseNo(actorProfileRequest.getGranted()));
		actorProfileRequest.setActorAsString(String.format("%s %s(%s)", getAsString(array, index++),getAsString(array, index++),getAsString(array, index++)));
		actorProfileRequest.setProfileAsString(getAsString(array, index++));		
	}
}