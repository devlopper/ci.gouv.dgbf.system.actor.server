package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true)
public class ProfileNumberOfActorsReader extends AbstractProfileReaderImpl implements Serializable {

	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t.profile",Profile.FIELD_IDENTIFIER).add("COUNT(t.actor.identifier)");
		arguments.getTuple(Boolean.TRUE).add("ActorProfile t");
		arguments.getPredicate(Boolean.TRUE).add("t.profile.identifier IN :"+Querier.PARAMETER_NAME_IDENTIFIERS);
		arguments.getGroup(Boolean.TRUE).add("t.profile.identifier");
		return QueryStringBuilder.getInstance().build(arguments);
	}
	
	@Override
	protected void __set__(Profile profile, Object[] array) {
		Integer index = 1;
		profile.setNumberOfActors(NumberHelper.getInteger(array[index],0));
	}
	
	@Override
	public void set(Collection<Profile> profiles, Collection<Object[]> arrays) {
		super.set(profiles, arrays);
		profiles.forEach(profile -> {
			if(profile.getNumberOfActors() == null)
				profile.setNumberOfActors(0);
		});
	}
}