package ci.gouv.dgbf.system.actor.server.representation.entities;

import java.io.Serializable;
import java.util.ArrayList;

import org.cyk.utility.__kernel__.object.__static__.representation.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableAuditedImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
public class ProfileDto extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableAuditedImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private ProfileTypeDto type;
	private String typeIdentifier;
	private String typeAsString;
	private ArrayList<String> privilegesAsStrings;
	private String profileIdentifier;
	
	private ArrayList<String> creatablePrivilegesIdentifiers;
	private ArrayList<String> deletablePrivilegesIdentifiers;
	
	private Byte orderNumber;
	private Boolean requestable;
	private String requestableAsString;
	
	private Boolean used;
	private String usedAsString;
	private Integer numberOfActors;
	
	public static final String JSON_FIELD_IDENTIFIER = "identifiant";
	public static final String JSON_FIELD_CODE = "code";
	public static final String JSON_FIELD_NAME = "libelle";
}