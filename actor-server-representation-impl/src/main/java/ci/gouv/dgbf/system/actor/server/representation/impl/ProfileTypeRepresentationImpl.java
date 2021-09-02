package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.map.CollectionOfMapsStringStringBuilder;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ProfileTypeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;
import ci.gouv.dgbf.system.actor.server.representation.api.ProfileTypeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ProfileTypeDto;

@ApplicationScoped
public class ProfileTypeRepresentationImpl extends AbstractRepresentationEntityImpl<ProfileTypeDto> implements ProfileTypeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
	public static Response get(Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {
					@Override
					public void run() {
						Collection<ProfileType> profileTypes = __inject__(ProfileTypeBusiness.class).get(pageable, firstTupleIndex, numberOfTuples);
						CollectionOfMapsStringStringBuilder.Arguments<ProfileType> arguments = new CollectionOfMapsStringStringBuilder.Arguments<ProfileType>()
								.setCollection(profileTypes).addFieldsNames(ProfileType.FIELD_IDENTIFIER,ProfileTypeDto.JSON_FIELD_IDENTIFIER
										,ProfileType.FIELD_CODE,ProfileTypeDto.JSON_FIELD_CODE,ProfileType.FIELD_NAME,ProfileTypeDto.JSON_FIELD_NAME);
						arguments.setEmptyValueAsArrayList();
						responseBuilderArguments.setEntity(CollectionOfMapsStringStringBuilder.getInstance().build(ProfileType.class, arguments));
					}
				};
			}
		});
	}
}