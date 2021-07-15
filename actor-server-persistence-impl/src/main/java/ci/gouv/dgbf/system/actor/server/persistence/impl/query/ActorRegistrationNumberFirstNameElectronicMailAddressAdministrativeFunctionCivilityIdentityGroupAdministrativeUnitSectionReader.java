package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Civility;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentityGroup;

public class ActorRegistrationNumberFirstNameElectronicMailAddressAdministrativeFunctionCivilityIdentityGroupAdministrativeUnitSectionReader extends AbstractActorReaderImpl implements Serializable {

	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Actor.FIELD_IDENTIFIER);
		arguments.getProjection(Boolean.TRUE).addFromTuple("i", Identity.FIELD_REGISTRATION_NUMBER,Identity.FIELD_FIRST_NAME,Identity.FIELD_LAST_NAMES
				,Identity.FIELD_ELECTRONIC_MAIL_ADDRESS,Identity.FIELD_ADMINISTRATIVE_FUNCTION);
		arguments.getProjection(Boolean.TRUE).addFromTuple("c", Civility.FIELD_NAME);
		arguments.getProjection(Boolean.TRUE).addFromTuple("ig", IdentityGroup.FIELD_NAME);
		arguments.getProjection(Boolean.TRUE).add(Language.Select.concatCodeName("au"),Language.Select.concatCodeName("s"));
		arguments.getTuple(Boolean.TRUE).add("Actor t").addJoins(
				"JOIN Identity i ON i = t.identity",
				"LEFT JOIN Civility c ON c = i.civility",
				"LEFT JOIN IdentityGroup ig ON ig = i.group",
				"LEFT JOIN AdministrativeUnit au ON au = i.administrativeUnit",
				"LEFT JOIN Section s ON s = au.section"
				);
		arguments.getPredicate(Boolean.TRUE).add("t.identifier IN :"+Querier.PARAMETER_NAME_IDENTIFIERS);
		return QueryStringBuilder.getInstance().build(arguments);
	}
	
	@Override
	protected void __set__(Actor actor, Object[] array) {
		Integer index = 1;
		//Identity Join
		actor.setRegistrationNumber(getAsString(array, index++));
		actor.setFirstName(getAsString(array, index++));
		actor.setLastNames(getAsString(array, index++));		
		actor.setElectronicMailAddress(getAsString(array, index++));
		actor.setAdministrativeFunction(getAsString(array, index++));
		//Other Joins
		actor.setCivilityAsString(getAsString(array, index++));
		actor.setGroupAsString(getAsString(array, index++));
		actor.setAdministrativeUnitAsString(getAsString(array, index++));
		actor.setSectionAsString(getAsString(array, index++));	
	}
}