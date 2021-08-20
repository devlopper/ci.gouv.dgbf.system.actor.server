package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true)
public class ProfileTypeAsStringsReader extends AbstractProfileReaderImpl implements Serializable {

	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Profile.FIELD_IDENTIFIER,FieldHelper.join(Profile.FIELD_TYPE,ProfileType.FIELD_NAME));
		arguments.getTuple(Boolean.TRUE).add("Profile t");
		arguments.getPredicate(Boolean.TRUE).add("t.identifier IN :"+Querier.PARAMETER_NAME_IDENTIFIERS);
		return QueryStringBuilder.getInstance().build(arguments);
	}
	
	@Override
	protected void __set__(Profile profile, Object[] array) {
		Integer index = 1;
		profile.setTypeAsString(getAsString(array, index));
	}
}